### initial settings
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/taa/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/taa/')
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
library(caTools)
library(fredr)
library(TTR)
library(BatchGetSymbols)

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
# v_color <- as.character('H') # turbo

v_c_alpha <- 1.000
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

### gathering data

start_date <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52*5), unit = 'week')
end_date  <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(0), unit = 'week')

df_ibov <- tq_get("^BVSP", from = start_date, to = end_date)
df_ibov$date <- lubridate::ymd(df_ibov$date)

tickers <- fix.ticker.name(GetIbovStocks()$tickers)

df_stocks <- BatchGetSymbols::BatchGetSymbols(tickers = tickers,
                                              first.date = start_date,
                                              last.date = end_date,
                                              freq.data = 'daily',
                                              bench.ticker = '^BVSP')

df_prices <- df_stocks$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  as_tibble()

colnames(df_prices) <- c('date','ticker','adjusted')
df_prices$date <- lubridate::ymd(df_prices$date)

df_ibov_returns <- df_ibov %>%
  dplyr::select(date,adjusted) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(lrets = TTR::ROC(adjusted)) %>% na.omit() %>%
  dplyr::mutate(return = exp(cumsum(lrets)) - 1) %>%
  dplyr::select(-c(adjusted,lrets))

df_returns <- df_prices %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(lrets = TTR::ROC(adjusted)) %>% na.omit() %>%
  dplyr::mutate(return = exp(cumsum(lrets)) - 1) %>%
  dplyr::select(-c(adjusted,lrets))

df_returns %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.250) +
  geom_line(data = df_spx_returns, mapping = aes(x = date, y = return), size = 2, color = 'black') +
  geom_text(data = filter(df_ibov_returns, date == tail(date, n =1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(6)), y = return), size = 4.25, fontface = 'bold') +
  labs(title = 'IBovespa: All Stocks', subtitle = 'Cumulative Returns (5 Years)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(date_breaks = '16 weeks', date_labels = "%d %b") +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

n_short <- 52

df_ibov_returns_short <- df_ibov %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(n_short))) %>%
  dplyr::select(date,adjusted) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(lrets = TTR::ROC(adjusted)) %>% na.omit() %>%
  dplyr::mutate(return = exp(cumsum(lrets)) - 1) %>%
  dplyr::select(-c(adjusted,lrets))

df_returns_short <- df_prices %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(lrets = TTR::ROC(adjusted)) %>% na.omit() %>%
  dplyr::mutate(return = exp(cumsum(lrets)) - 1) %>%
  dplyr::select(-c(adjusted,lrets))

df_returns_short %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.250) +
  geom_line(data = df_spx_returns_short, mapping = aes(x = date, y = return), size = 2, color = 'black') +
  geom_text(data = filter(df_spx_returns_short, date == tail(date, n =1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.25, fontface = 'bold') +
  labs(title = 'IBovespa: All Stocks', subtitle = 'Cumulative Returns (52 weeks)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(date_breaks = '4 weeks', date_labels = "%d %b") +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)


