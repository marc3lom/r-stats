### initial settings
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/economics/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/economics/')
# setwd('/home/marcelom/.local/code/r/economics/')

# locale settings
# windows
# Sys.setlocale('LC_ALL', 'English')
# Sys.setlocale('LC_TIME', 'Portuguese')
# linux
# Sys.setlocale('LC_ALL', 'en_US.UTF-8')
# Sys.setlocale('LC_TIME', 'pt_BR.UTF-8')

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

library(dotenv)
# load_dot_env(file = 'd:/usr/marcelom/OneDrive/Code/R/.env')
# load_dot_env(file = '/home/marcelom/.env')
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

v_c_alpha <- 0.750 # set value between 0 and 1
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

# getting and preparing data
# by fred
df_h_cpi <- fredr(
  series_id = 'CPIAUCSL',
  observation_start = as.Date('1957-01-01'),
  observation_end = as.Date(Sys.Date()),
  frequency = 'm'
) %>%
  subset(select = c(date,series_id,value))

df_c_cpi <- fredr(
  series_id = 'CPILFESL',
  observation_start = as.Date('1957-01-01'),
  observation_end = as.Date(Sys.Date()),
  frequency = 'm'
) %>%
  subset(select = c(date,series_id,value))

df_h_pce <- fredr(
  series_id = 'PCEPI',
  observation_start = as.Date('1957-01-01'),
  observation_end = as.Date(Sys.Date()),
  frequency = 'm'
) %>%
  subset(select = c(date,series_id,value))

df_c_pce <- fredr(
  series_id = 'PCEPILFE',
  observation_start = as.Date('1957-01-01'),
  observation_end = as.Date(Sys.Date()),
  frequency = 'm'
) %>%
  subset(select = c(date,series_id,value))

df_cpi <- rbind(df_h_cpi,df_c_cpi) %>%
  pivot_longer(cols = -c(date,series_id), names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(series_id) %>% dplyr::arrange(date) %>%
  pivot_wider(names_from = series_id, values_from = values) %>% 
  subset(select = c(date,CPIAUCSL,CPILFESL))

df_pce <- rbind(df_h_pce,df_c_pce) %>%
  pivot_longer(cols = -c(date,series_id), names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(series_id) %>% dplyr::arrange(date) %>%
  pivot_wider(names_from = series_id, values_from = values) %>% 
  subset(select = c(date,PCEPI,PCEPILFE))

colnames(df_cpi) <- tolower(colnames(df_cpi))
colnames(df_pce) <- tolower(colnames(df_pce))

# modifying data

df_cpi <- df_cpi %>% 
  dplyr::mutate(head_cpi_mom = exp(TTR::ROC(cpiaucsl)) - 1) %>%
  dplyr::mutate(head_cpi_yoy = exp(TTR::ROC(cpiaucsl, n = 12)) - 1) %>%
  dplyr::mutate(core_cpi_mom = exp(TTR::ROC(cpilfesl)) - 1) %>%
  dplyr::mutate(core_cpi_yoy = exp(TTR::ROC(cpilfesl, n = 12)) - 1) %>% na.omit()

df_cpi_mom <- df_cpi %>%
  subset(select = c(date,head_cpi_mom,core_cpi_mom)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_cpi_yoy <- df_cpi %>%
  subset(select = c(date,head_cpi_yoy,core_cpi_yoy)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_pce <- df_pce %>% 
  dplyr::mutate(head_pce_mom = exp(TTR::ROC(pcepi)) - 1) %>%
  dplyr::mutate(head_pce_yoy = exp(TTR::ROC(pcepi, n = 12)) - 1) %>%
  dplyr::mutate(core_pce_mom = exp(TTR::ROC(pcepilfe)) - 1) %>%
  dplyr::mutate(core_pce_yoy = exp(TTR::ROC(pcepilfe, n = 12)) - 1) %>% na.omit()

df_pce_mom <- df_pce %>%
  subset(select = c(date,head_pce_mom,core_pce_mom)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_pce_yoy <- df_pce %>%
  subset(select = c(date,head_pce_yoy,core_pce_yoy)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_px_idx <- merge(df_cpi,df_pce, by = 'date')

df_px_idx_mom <- df_px_idx %>%
  subset(select = c(date,head_cpi_mom,core_cpi_mom,head_pce_mom,core_pce_mom)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_px_idx_yoy <- df_px_idx %>%
  subset(select = c(date,head_cpi_yoy,core_cpi_yoy,head_pce_yoy,core_pce_yoy)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')
  
### GGPlots

p0_cpi <- df_cpi_mom %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 0.750) +
  labs(title = "US: Consumer Price Index", subtitle = NULL, caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(name = '%chg mom') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ticker, ncol = 1)

p0_pce <- df_pce_mom %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 0.750) +
  labs(title = "US: Personal Consumption Expenditures", subtitle = NULL, caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(name = '%chg mom') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ticker, ncol = 1)

p0_idx <- df_px_idx_mom %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 0.750) +
  labs(title = "US: Price Indexes", subtitle = NULL, caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(name = '%chg mom') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ticker, ncol = 2)

p1_cpi <- df_cpi_yoy %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 0.875) +
  geom_text(data = filter(df_cpi, date == tail(date, n = 1)), aes(label = percent(head_cpi_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = head_cpi_yoy), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_cpi, date == tail(date, n = 1)), aes(label = percent(core_cpi_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = core_cpi_yoy), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0.00, size = 0.625, color = "orangered", alpha = v_f_alpha) +
  geom_hline(yintercept = 0.02, size = 0.625, color = "gray32", alpha = v_f_alpha, linetype = 'dashed') +
  labs(title = "US: Consumer Price Index", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(limits = c(-0.025,.175),breaks = seq(0,.15,.05), name = '%chg yoy') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p1_pce <- df_pce_yoy %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 0.875) +
  geom_text(data = filter(df_pce, date == tail(date, n = 1)), aes(label = percent(head_pce_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = head_pce_yoy), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_pce, date == tail(date, n = 1)), aes(label = percent(core_pce_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = core_pce_yoy), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0.00, size = 0.625, color = "orangered", alpha = v_f_alpha) +
  geom_hline(yintercept = 0.02, size = 0.625, color = "gray32", alpha = v_f_alpha, linetype = 'dashed') +
  labs(title = "US: Personal Consumption Expenditures", subtitle = NULL, caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(limits = c(-0.025,.175),breaks = seq(0,.15,.05), name = '%chg yoy') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p1_idx <- df_px_idx_yoy %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 0.875) +
  geom_text(data = filter(df_cpi, date == tail(date, n = 1)), aes(label = percent(head_cpi_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = head_cpi_yoy), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_cpi, date == tail(date, n = 1)), aes(label = percent(core_cpi_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = core_cpi_yoy), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_pce, date == tail(date, n = 1)), aes(label = percent(head_pce_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = head_pce_yoy), size = 4.25, fontface = "bold") +
  geom_text(data = filter(df_pce, date == tail(date, n = 1)), aes(label = percent(core_pce_yoy, accuracy = .01), x = as.Date(Sys.Date() + lubridate::weeks(52*2)), y = core_pce_yoy), size = 4.25, fontface = "bold") +
  geom_hline(yintercept = 0.00, size = 0.625, color = "orangered", alpha = v_f_alpha) +
  geom_hline(yintercept = 0.02, size = 0.625, color = "gray32", alpha = v_f_alpha, linetype = 'dashed') +
  labs(title = "US: Price Indexes", subtitle = NULL, caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(name = '%chg yoy') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

grid.arrange(p1_cpi,p1_pce,ncol=1)
