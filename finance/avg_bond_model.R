### initial settings
rm(list = ls(all = TRUE))
par(mfrow = c(1, 1))

# setwd('g:/Diest/marcelom/R/taa/')
# setwd("d:/usr/marcelom/OneDrive/Code/R/taa/")
setwd('/home/marcelom/.local/code/r/finance/')

# locale settings
# windows
# Sys.setlocale("LC_ALL", "English")
# Sys.setlocale("LC_TIME", "Portuguese")
# linux
Sys.setlocale('LC_ALL', 'en_US.UTF-8')
Sys.setlocale('LC_TIME', 'pt_BR.UTF-8')

### calling libraries

library(tidyverse)
library(tidyquant)
library(tibbletime)
library(reshape2)
library(hrbrthemes)
library(ggsci)
library(readxl)
library(ggpubr)
library(quantmod)
library(quantreg)
library(forecast)
library(patchwork)
library(grid)
library(gridExtra)
library(scales)
library(texreg)
library(timetk)
library(caTools)
library(moments)
library(MASS)
library(metRology)
library(stringr)
library(fredr)
library(TTR)

library(dotenv)
# load_dot_env(file = "d:/usr/marcelom/OneDrive/Code/R/.env")
load_dot_env(file = '/home/marcelom/.env')
# calling .env variable example
fredr_set_key(Sys.getenv("fred_api_key"))
# quandl_api_key(Sys.getenv('quandl_key'))

### ggplot2 settings

theme_set(
  theme_minimal()
)

theme_update(
  legend.position = "none",
  legend.title = element_blank(),
  legend.text = element_text(face = "bold"),
  plot.caption = element_text(face = "bold"),
  axis.text.x = element_text(face = "bold"),
  axis.text.y = element_text(face = "bold")
)

# viridis options
library(viridisLite)
library(viridis)

# v_color <- as.character('A') # magma
# v_color <- as.character('B') # inferno
# v_color <- as.character('C') # plasma
v_color <- as.character('D') # viridis
# v_color <- as.character('E') # cividis
# v_color <- as.character('F') # rocket
# v_color <- as.character('G') # mako
# v_color <- as.character("H") # turbo

v_c_alpha <- 1.000 # set value between 0 and 1
v_f_alpha <- v_c_alpha * 0.750

### custom functions

# moving average
mavg <- function(x, n = 7, s = 1) {
  stats::filter(
    x,
    rep(
      1 / ifelse(s == 1, n, ifelse(n %% 2 == 0, n + 1, n)),
      ifelse(s == 1, n, ifelse(n %% 2 == 0, n + 1, n))
    ),
    sides = s
  )
}

# geometric mean using logs
gm_mean <- function(x) {
  exp(
    mean(
      log(x[is.finite(log(x))])
    )
  )
}

# linear model equation and r-squared display


### gathering data
# by spreadsheet
# df <- read_xlsx("d:/usr/marcelom/OneDrive/Code/R/taa/bond_model.xlsx", sheet = "dataRead")
# df <- read_xlsx("/home/marcelom/.local/code/r/finance/bond_model.xlsx", sheet = "dataRead")

# by fred
# getting motnhly data

start_date <- as.Date("1962-01-01")
end_date <- lubridate::floor_date(Sys.Date(), unit = "month")

df_dgs1 <- fredr(
  series_id = "DGS1",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m",
  aggregation_method = "avg"
) %>% subset(select = c(date, series_id, value))

df_dgs10 <- fredr(
  series_id = "DGS10",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m",
  aggregation_method = "avg"
) %>% subset(select = c(date, series_id, value))

df_hcpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
) %>% subset(select = c(date, series_id, value))

df_dgs1 <- df_dgs1[-c(nrow(df_dgs1)),]
df_dgs1$date <- lubridate::ymd(df_dgs1$date)
df_dgs10 <- df_dgs10[-c(nrow(df_dgs10)),]
df_dgs10$date <- lubridate::ymd(df_dgs10$date)
df_hcpi$date <- lubridate::ymd(df_hcpi$date)

# gdpa data

df_ngdp_q <- fredr(
  series_id = "GDP",
  observation_start = as.Date("1962-01-01"),
  observation_end = lubridate::floor_date(Sys.Date(), unit = "quarter"),
  frequency = "q"
) %>% subset(select = c(date, series_id, value))

df_ngdp_q$date <- lubridate::ymd(df_ngdp_q$date)
df_ngdp_q$date <- df_ngdp_q$date %m+% months(3)

df_rgdp_q <- fredr(
  series_id = "GDPC1",
  observation_start = as.Date("1962-01-01"),
  observation_end = lubridate::floor_date(Sys.Date(), unit = "quarter"),
  frequency = "q"
) %>% subset(select = c(date, series_id, value))

df_rgdp_q$date <- lubridate::ymd(df_rgdp_q$date)
df_rgdp_q$date <- df_rgdp_q$date %m+% months(3)

monthly_ngdp <- seq(df_ngdp_q$date[1], tail(df_ngdp_q$date, 1), by = "month")
monthly_rgdp <- seq(df_rgdp_q$date[1], tail(df_rgdp_q$date, 1), by = "month")
ngdp_q <- df_ngdp_q[c("date", "value")]
rgdp_q <- df_rgdp_q[c("date", "value")]
df_ngdp_m <- tibble(date = monthly_ngdp, df_ngdp_m = spline(ngdp_q, method = "fmm", xout = monthly_ngdp)$y)
df_rgdp_m <- tibble(date = monthly_rgdp, df_rgdp_m = spline(rgdp_q, method = "fmm", xout = monthly_rgdp)$y)
df_ngdp_m$series_id <- as.character(df_ngdp_q$series_id[1])
df_rgdp_m$series_id <- as.character(df_rgdp_q$series_id[1])
colnames(df_ngdp_m) <- c("date", "value", "series_id")
colnames(df_rgdp_m) <- c("date", "value", "series_id")

# binding data frames

df_m1 <- rbind(df_dgs10, df_dgs1, df_ngdp_m) %>%
  pivot_longer(cols = -c(date, series_id), names_to = "ticker", values_to = "values") %>%
  dplyr::group_by(series_id) %>%
  dplyr::arrange(date) %>%
  pivot_wider(names_from = series_id, values_from = values) %>%
  subset(select = c(date, DGS10, DGS1, GDP))

df_m2 <- rbind(df_dgs10, df_hcpi, df_rgdp_m) %>%
  pivot_longer(cols = -c(date, series_id), names_to = "ticker", values_to = "values") %>%
  dplyr::group_by(series_id) %>%
  dplyr::arrange(date) %>%
  pivot_wider(names_from = series_id, values_from = values) %>%
  subset(select = c(date, DGS10, CPIAUCSL, GDPC1))

colnames(df_m1) <- tolower(colnames(df_m1))
colnames(df_m2) <- tolower(colnames(df_m2))
df_m1$date <- lubridate::ymd(df_m1$date)
df_m2$date <- lubridate::ymd(df_m2$date)

# calculating models

num_years <- 10
avg_factor <- 12 # 1 for years, 4 for quarters, 12 for months, 52 for weeks and 252 for days
num_per <- num_years * avg_factor # setting the averaging rolling window periods

df_model_1 <- df_m1 %>%
  dplyr::mutate(us_10y = dgs10) %>%
  dplyr::mutate(us_01y = mavg(dgs1, num_per)) %>%
  dplyr::mutate(ngdp_10y = mavg((exp(TTR::ROC(gdp))^avg_factor) - 1, num_per) * 100) %>%
  dplyr::mutate(model1_10y = (us_01y + ngdp_10y) / 2) %>%
  dplyr::mutate(model1_error = us_10y - model1_10y) %>% na.omit()

df_model_2 <- df_m2 %>%
  dplyr::mutate(us_10y = dgs10) %>%
  dplyr::mutate(hcpi_10y = mavg((exp(TTR::ROC(cpiaucsl))^avg_factor) - 1, num_per) * 100) %>%
  dplyr::mutate(rgdp_10y = mavg((exp(TTR::ROC(gdpc1))^avg_factor) - 1, num_per) * 100) %>%
  dplyr::mutate(model2_10y = (hcpi_10y + rgdp_10y) / 2) %>%
  dplyr::mutate(model2_error = us_10y - model2_10y) %>% na.omit()

df_model_avg <- merge(df_model_1,df_model_2,by='date') %>%
  subset(select = c(date, us_10y.x, model1_10y, model2_10y)) %>%
  dplyr::mutate(us_10y = us_10y.x) %>% subset(select = c(date, us_10y, model1_10y, model2_10y)) %>%
  dplyr::mutate(model_avg = (model1_10y + model2_10y) / 2) %>%
  dplyr::mutate(model_avg_error = us_10y - model_avg) %>% na.omit()

fit_lm1 <- lm(us_10y ~ us_01y + ngdp_10y, data = df_model_1)
fit_lm2 <- lm(us_10y ~ hcpi_10y + rgdp_10y, data = df_model_2)

fit_arima <- xts(x = df_model_1$us_10y, order.by = df_model_1$date) %>%
  auto.arima()

# GGPlots

# Model 1 (US 1 Year Treasury and Nominal GDP)
p1_m1 <- df_model_1 %>%
  dplyr::select(date, model1_10y, us_01y, us_10y, ngdp_10y) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_model_1, date == tail(date, n = 1)), aes(label = comma(model1_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model1_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model_1, date == tail(date, n = 1)), aes(label = comma(ngdp_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = ngdp_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model_1, date == tail(date, n = 1)), aes(label = comma(us_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = us_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model_1, date == tail(date, n = 1)), aes(label = comma(us_01y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = us_01y), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = "Modelo US 10Y Treasury", subtitle = "US01Y | PIB Nominal", caption = NULL, x = NULL, y = "(%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2_m1 <- df_model_1 %>%
  dplyr::select(date, model1_error) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_model_1, date == tail(date, n = 1)), aes(label = comma(model1_error, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model1_error), size = 4.25, fontface = "bold") +
  geom_text(data = df_model_1, aes(label = comma(mean(model1_error), accuracy = .001), x = as.Date('1990-01-01'), y = 8), size = 4.25, fontface = "bold") +
  geom_text(data = df_model_1, aes(label = comma(sd(model1_error), accuracy = .001), x = as.Date('2000-01-01'), y = 8), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL, y = "Erro (%)") +
  scale_x_date() +
  scale_y_comma(limits = c(-5,11), breaks = seq(-4,10,2), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# Model 2 (US CPI and Real GDP)
p1_m2 <- df_model_2 %>%
  dplyr::select(date, model2_10y, us_10y, hcpi_10y, rgdp_10y) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_model_2, date == tail(date, n = 1)), aes(label = comma(model2_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model2_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model_2, date == tail(date, n = 1)), aes(label = comma(rgdp_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = rgdp_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model_2, date == tail(date, n = 1)), aes(label = comma(us_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = us_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model_2, date == tail(date, n = 1)), aes(label = comma(hcpi_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = hcpi_10y), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = "Modelo US 10Y Treasury", subtitle = "US CPI | PIB Real", caption = NULL, x = NULL, y = "(%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2_m2 <- df_model_2 %>%
  dplyr::select(date, model2_error) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_model_2, date == tail(date, n = 1)), aes(label = comma(model2_error, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model2_error), size = 4.25, fontface = "bold") +
  geom_text(data = df_model_2, aes(label = comma(mean(model2_error), accuracy = .001), x = as.Date('1990-01-01'), y = 8), size = 4.25, fontface = "bold") +
  geom_text(data = df_model_2, aes(label = comma(sd(model2_error), accuracy = .001), x = as.Date('2000-01-01'), y = 8), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL, y = "Erro (%)") +
  scale_x_date() +
  scale_y_comma(limits = c(-5,11), breaks = seq(-4,10,2), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# Model Averages
p1_m_avg <- df_model_avg %>%
  dplyr::select(date, us_10y, model1_10y,model2_10y,model_avg) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_model_avg, date == tail(date, n = 1)), aes(label = comma(model_avg, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model_avg), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model_avg, date == tail(date, n = 1)), aes(label = comma(us_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = us_10y), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = "Modelo US 10Y Treasury", subtitle = "Media dos Modelos", caption = NULL, x = NULL, y = "(%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

df_avg_resid <- df_model_avg %>%
  dplyr::select(date, model_avg_error) %>%
  dplyr::mutate(model_avg_resid = model_avg_error) %>%
  dplyr::select(date, model_avg_resid)
df_avg_resid$model_1_resid <- df_model_1$model1_error
df_avg_resid$model_2_resid <- df_model_2$model2_error

p2_m_avg <- df_avg_resid %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_avg_resid, date == tail(date, n = 1)), aes(label = comma(model_avg_resid, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model_avg_resid), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_avg_resid, date == tail(date, n = 1)), aes(label = comma(model_1_resid, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model_1_resid), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_avg_resid, date == tail(date, n = 1)), aes(label = comma(model_2_resid, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model_2_resid), size = 4.25, fontface = "bold") +
  geom_text(data = df_avg_resid, aes(label = paste('erro medio',comma(mean(model_avg_resid), accuracy = .001),sep = '\n'), x = as.Date('1990-01-01'), y = 8), size = 4.25, fontface = "bold") +
  geom_text(data = df_avg_resid, aes(label = paste('desvio padrao',comma(sd(model_avg_resid), accuracy = .001),sep = '\n'), x = as.Date('2000-01-01'), y = 8), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL, y = "Erro (%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(limits = c(-5,11), breaks = seq(-4,10,2), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# Fitted Models
# Modeled vs Fitted
df_fitted <- df_model_1 %>%
  dplyr::select(date, model1_10y, us_10y)
df_fitted$model2_10y <- df_model_2$model2_10y
df_fitted$fitted_10y <- fit_lm1$fitted.values
df_fitted_residuals <- df_model_1 %>%
  dplyr::select(date)
df_fitted_residuals$model1_residuals <- df_model_1$model1_error
df_fitted_residuals$model2_residuals <- df_model_2$model2_error
df_fitted_residuals$fitted_residuals <- fit_lm1$residuals

p1_fitted <- df_fitted %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_fitted, date == tail(date, n = 1)), aes(label = comma(model1_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model1_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_fitted, date == tail(date, n = 1)), aes(label = comma(model2_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model2_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_fitted, date == tail(date, n = 1)), aes(label = comma(fitted_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = fitted_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_fitted, date == tail(date, n = 1)), aes(label = comma(us_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = us_10y), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = "Modelos US 10Y Treasury", subtitle = "(US 1Y | PIB Nominal),(US CPI | PIB Real), Fittado", caption = NULL, x = NULL, y = "(%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2_fitted <- df_fitted_residuals %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_fitted_residuals, date == tail(date, n = 1)), aes(label = comma(model1_residuals, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model1_residuals), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_fitted_residuals, date == tail(date, n = 1)), aes(label = comma(model2_residuals, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model2_residuals), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_fitted_residuals, date == tail(date, n = 1)), aes(label = comma(fitted_residuals, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = fitted_residuals), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL, y = "Erro (%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(limits = c(-5,11), breaks = seq(-4,10,2), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# ARIMA model
df_arima_fit <- df_model_1 %>%
  dplyr::select(date, us_10y,model1_10y)
df_arima_fit$model2_10y <- df_model_2$model2_10y
df_arima_fit$arima_10y <- fit_arima$fitted
df_arima_resid <- df_model_1 %>%
  dplyr::select(date)
df_arima_resid$model1_resid <- df_model_1$model1_error
df_arima_resid$model2_resid <- df_model_2$model2_error
df_arima_resid$arima_resid <- fit_arima$residuals

p1_arima <- df_arima_fit %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_arima_fit, date == tail(date, n = 1)), aes(label = comma(us_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = us_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_arima_fit, date == tail(date, n = 1)), aes(label = comma(model1_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model1_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_arima_fit, date == tail(date, n = 1)), aes(label = comma(model2_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model2_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_arima_fit, date == tail(date, n = 1)), aes(label = comma(arima_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = arima_10y), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = "Modelo US 10Y Treasury", subtitle = "US01Y | PIB Nominal | ARIMA", caption = NULL, x = NULL, y = "(%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2_arima <- df_arima_resid %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_arima_resid, date == tail(date, n = 1)), aes(label = comma(model1_resid, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model1_resid), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_arima_resid, date == tail(date, n = 1)), aes(label = comma(model2_resid, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = model2_resid), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_arima_resid, date == tail(date, n = 1)), aes(label = comma(arima_resid, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52 * 3 / 2)), y = arima_resid), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL, y = "Erro (%)") +
  theme(legend.position = "top") +
  scale_x_date() +
  scale_y_comma(limits = c(-5,11), breaks = seq(-4,10,2), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# Setting up Plot Grids and Display of Plots

# model 1
g1 <- list(p1_m1, p2_m1)
grid.arrange(
  grobs = g1,
  widths = rep(1, times = 2),
  layout_matrix = rbind(
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(2, times = 2)),
    c(rep(2, times = 2))
  )
)

# model 2
g1 <- list(p1_m2, p2_m2)
grid.arrange(
  grobs = g1,
  widths = rep(1, times = 2),
  layout_matrix = rbind(
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(2, times = 2)),
    c(rep(2, times = 2))
  )
)

# model averages
g1 <- list(p1_m_avg, p2_m_avg)
grid.arrange(
  grobs = g1,
  widths = rep(1, times = 2),
  layout_matrix = rbind(
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(2, times = 2)),
    c(rep(2, times = 2))
  )
)

# models versus fit
g1 <- list(p1_fitted, p2_fitted)
grid.arrange(
  grobs = g1,
  widths = rep(1, times = 2),
  layout_matrix = rbind(
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(2, times = 2)),
    c(rep(2, times = 2))
  )
)

# models versus arima
g1 <- list(p1_arima, p2_arima)
grid.arrange(
  grobs = g1,
  widths = rep(1, times = 2),
  layout_matrix = rbind(
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(1, times = 2)),
    c(rep(2, times = 2)),
    c(rep(2, times = 2))
  )
)



