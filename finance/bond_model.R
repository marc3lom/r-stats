### initial settings
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/taa/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/taa/')
setwd('/home/marcelom/.local/code/r/finance/')

# locale settings
# windows
# Sys.setlocale('LC_ALL', 'English')
# Sys.setlocale('LC_TIME', 'Portuguese')
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
# load_dot_env(file = 'd:/usr/marcelom/OneDrive/Code/R/.env')
load_dot_env(file = '/home/marcelom/.env')
# calling .env variable example
fredr_set_key(Sys.getenv('fred_api_key'))
# quandl_api_key(Sys.getenv('quandl_key'))

### ggplot2 settings

theme_set(
  theme_minimal()
)

theme_update(
  legend.position = 'none',
  legend.title = element_blank(),
  legend.text = element_text(face = 'bold'),
  plot.caption = element_text(face = 'bold'),
  axis.text.x = element_text(face = 'bold'),
  axis.text.y = element_text(face = 'bold')
)

# viridis options
library(viridisLite)
library(viridis)

# v_color <- as.character('A') # magma
# v_color <- as.character('B') # inferno
# v_color <- as.character('C') # plasma
# v_color <- as.character('D') # viridis
# v_color <- as.character('E') # cividis
# v_color <- as.character('F') # rocket
# v_color <- as.character('G') # mako
v_color <- as.character('H') # turbo

v_c_alpha <- 1.000 # set value between 0 and 1
v_f_alpha <- v_c_alpha * 0.750

### custom functions

# moving average
mavg <- function(x,n=7,s=1){
  stats::filter(
    x,
    rep(
      1/ifelse(s == 1,n,ifelse(n %% 2 == 0,n + 1,n)),
      ifelse(s == 1,n,ifelse(n %% 2 == 0,n + 1,n))
    ),
    sides=s
  )
}

# geometric mean using logs
gm_mean <- function(x){
  exp(
    mean(
      log(x[is.finite(log(x))])
    )
  )
}

### gathering data
# by spreadsheet
# df <- read_xlsx("d:/usr/marcelom/OneDrive/Code/R/taa/bond_model.xlsx", sheet = "dataRead")
# df <- read_xlsx("/home/marcelom/.local/code/r/finance/bond_model.xlsx", sheet = "dataRead")

# by fred
df_dgs1 <- fredr(
  series_id = 'DGS1',
  observation_start = as.Date('1962-01-01'),
  observation_end = lubridate::floor_date(Sys.Date(), unit = 'quarter'),
  frequency = 'q',
  aggregation_method = 'avg'
) %>% subset(select = c(date,series_id,value))
df_dgs1[nrow(df_dgs1),3] <- as.numeric(fredr(
  series_id = 'DGS1',
  observation_start = lubridate::floor_date(Sys.Date(), unit = 'quarter'),
  observation_end = lubridate::floor_date(Sys.Date(), unit = 'quarter'),
  frequency = 'd'
)[3])

df_dgs1$date <- lubridate::ymd(df_dgs1$date)

df_dgs10 <- fredr(
  series_id = 'DGS10',
  observation_start = as.Date('1962-01-01'),
  observation_end = lubridate::floor_date(Sys.Date(), unit = 'quarter'),
  frequency = 'q',
  aggregation_method = 'avg'
) %>% subset(select = c(date,series_id,value))
df_dgs10[nrow(df_dgs10),3] <- as.numeric(fredr(
  series_id = 'DGS10',
  observation_start = lubridate::floor_date(Sys.Date(), unit = 'quarter'),
  observation_end = lubridate::floor_date(Sys.Date(), unit = 'quarter'),
  frequency = 'd'
)[3])

df_dgs10$date <- lubridate::ymd(df_dgs10$date)

df_ngdp <- fredr(
  series_id = 'GDP',
  observation_start = as.Date('1962-01-01'),
  observation_end = lubridate::floor_date(Sys.Date(), unit = 'quarter'),
  frequency = 'q'
) %>% subset(select = c(date,series_id,value))

df_ngdp$date <- lubridate::ymd(df_ngdp$date)
df_ngdp$date <- df_ngdp$date %m+% months(3)

df <- rbind(df_dgs1,df_dgs10,df_ngdp) %>%
  pivot_longer(cols = -c(date,series_id), names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(series_id) %>% dplyr::arrange(date) %>%
  pivot_wider(names_from = series_id, values_from = values) %>% 
  subset(select = c(date,DGS1,DGS10,GDP))

colnames(df) <- tolower(colnames(df))

df$date <- lubridate::ymd(df$date)

n_qtr <- 40

df_model <- df %>%
  dplyr::mutate(us_10y = dgs10) %>%
  dplyr::mutate(us_01y = mavg(dgs1,n_qtr)) %>%
  dplyr::mutate(aret_ann_ngdp = (exp(TTR::ROC(gdp))^4) - 1) %>%
  dplyr::mutate(ngdp_10y = mavg(aret_ann_ngdp,n_qtr) * 100) %>%
  dplyr::mutate(model_10y = (us_01y + ngdp_10y)/2) %>%
  dplyr::mutate(model_error = us_10y - model_10y) %>% na.omit()

p1 <- df_model %>%
  dplyr::select(date,us_10y,model_10y) %>%
  pivot_longer(cols = -date,
               names_to = 'ticker',
               values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.250) +
  geom_text(data = filter(df_model, date == tail(date, n = 1)), aes(label = comma(model_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52*3/2)), y = model_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model, date == tail(date, n = 1)), aes(label = comma(us_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52*3/2)), y = us_10y), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = "Modelo US Treasury", subtitle = 'US10Y vs. PIB Nominal e Yields 1 Ano', caption = NULL, x = NULL, y = 'Yield (%)') +
  theme(legend.position="top") +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2 <- df_model %>%
  dplyr::select(date,model_error) %>%
  pivot_longer(cols = -date,
               names_to = 'variable',
               values_to = 'value') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL, y = 'Erro (%)') +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p3 <- df_model %>%
  dplyr::select(date,model_10y,us_01y,us_10y,ngdp_10y) %>%
  pivot_longer(cols = -date,
               names_to = 'variable',
               values_to = 'value') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = value, color = variable, group = variable), size = 1.250) +
  geom_text(data = filter(df_model, date == tail(date, n = 1)), aes(label = comma(model_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52*3/2)), y = model_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model, date == tail(date, n = 1)), aes(label = comma(ngdp_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52*3/2)), y = ngdp_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model, date == tail(date, n = 1)), aes(label = comma(us_10y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52*3/2)), y = us_10y), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_model, date == tail(date, n = 1)), aes(label = comma(us_01y, accuracy = .001), x = as.Date(Sys.Date() + lubridate::weeks(52*3/2)), y = us_01y), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = "Modelo US 10Y Treasury", subtitle = 'US01Y | PIB Nominal', caption = NULL, x = NULL, y = '(%)') +
  theme(legend.position="top") +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

g1 <- list(p1,p2)
grid.arrange(
  grobs = g1,
  widths = rep(1,times = 2),
  layout_matrix = rbind(
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(2,times = 2)),
    c(rep(2,times = 2))
  )
)

g1 <- list(p3,p2)
grid.arrange(
  grobs = g1,
  widths = rep(1,times = 2),
  layout_matrix = rbind(
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(1,times = 2)),
    c(rep(2,times = 2)),
    c(rep(2,times = 2))
  )
)
