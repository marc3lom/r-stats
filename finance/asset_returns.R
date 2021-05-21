### initial settings
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/saa/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/saa/')
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
av_api_key(Sys.getenv('alpha_vantage_api_key'))
fredr_set_key(Sys.getenv('fred_api_key'))
# quandl_api_key(Sys.getenv('quandl_key'))

### ggplot2 settings

# theme_set(theme_bw())
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

# log returns
log_returns <- function(x,l=1) {
  y <- diff(log(x),l)
  y[is.infinite(y)] <- 0
}

### getting and preparing data

start_date <- lubridate::ymd(lubridate::floor_date(Sys.Date() - lubridate::weeks(52 * 25), unit = 'year') - lubridate::days(1))
end_date <- lubridate::ymd(Sys.Date())

tickers <- c('^GSPC','^NDX','^IXIC','^W5000','^FTSE','^GDAXI','^STOXX50E','^N225','^GSPTSE','^AORD')

df <- tq_get(
  tickers,
  get = 'stock.prices',
  from = start_date,
  to = end_date
)

df$date <- lubridate::ymd(df$date)

df <- df %>%
  dplyr::select(date,symbol,adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>% na.omit()

colnames(df) <- c('date','spx','ndx','ccmp','w5000','ukx','dax','sx5e','nky','tsx','as30')

df_rets <- df %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(l_rets = TTR::ROC(values)) %>% na.omit() %>%
  dplyr::mutate(a_rets = exp(l_rets) - 1) %>%
  dplyr::mutate(cl_rets = cumsum(l_rets)) %>%
  dplyr::mutate(ca_rets = exp(cl_rets) - 1)

df_l_rets <- df_rets %>%
  dplyr::select(date,ticker,l_rets) %>% na.omit() %>%
  pivot_wider(names_from = ticker, values_from = l_rets) %>% na.omit()

df_a_rets <- df_rets %>%
  dplyr::select(date,ticker,a_rets) %>% na.omit() %>%
  pivot_wider(names_from = ticker, values_from = a_rets) %>% na.omit()

df_cl_rets <- df_rets %>%
  dplyr::select(date,ticker,cl_rets) %>% na.omit() %>%
  pivot_wider(names_from = ticker, values_from = cl_rets) %>% na.omit()

df_ca_rets <- df_rets %>%
  dplyr::select(date,ticker,ca_rets) %>% na.omit() %>%
  pivot_wider(names_from = ticker, values_from = ca_rets) %>% na.omit()

df_ca_rets %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 0.625) +
  labs(title = 'G7: Equity', subtitle = 'Cumulative Returns', caption = NULL, x = NULL, y = NULL) +
  theme(legend.position = 'bottom') +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

