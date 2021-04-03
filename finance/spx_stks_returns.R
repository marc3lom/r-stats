rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd("g:/Diest/marcelom/R/taa/")
# setwd("c:/Hvymtl/usr/r/code/taa/")
# setwd('/home/marcelom/.local/code/r/')

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
library(BatchGetSymbols)
theme_set(theme_minimal())

# Moving Averages

n_st <- 3
n_mt <- 6
n_lt <- 12

mav_st <- function(x,n = n_st){stats::filter(x,rep(1/n,n), sides=1)}
mav_mt <- function(x,n = n_mt){stats::filter(x,rep(1/n,n), sides=1)}
mav_lt <- function(x,n = n_lt){stats::filter(x,rep(1/n,n), sides=1)}

mav_st_b <- function(x,n = 2*n_st+1){stats::filter(x,rep(1/n,n), sides=2)}
mav_mt_b <- function(x,n = 2*n_mt+1){stats::filter(x,rep(1/n,n), sides=2)}
mav_lt_b <- function(x,n = 2*n_lt+1){stats::filter(x,rep(1/n,n), sides=2)}

# Script Start

start_date <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52*100), unit = 'week')
end_date  <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(0), unit = 'week')

indices <- c('^GSPC','^IXIC','^RUT')

df_indx <- BatchGetSymbols::BatchGetSymbols(tickers = indices,
                                       first.date = start_date,
                                       last.date = end_date,
                                       freq.data = 'daily',
                                       thresh.bad.data = 0.60,
                                       do.cache = F)

start_date <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52*25), unit = 'week')

df_indx_short <- BatchGetSymbols::BatchGetSymbols(tickers = indices,
                                            first.date = start_date,
                                            last.date = end_date,
                                            freq.data = 'daily',
                                            thresh.bad.data = 0.85,
                                            do.cache = F)

tickers <- c('AAPL','MSFT','AMZN','GOOGL','TSLA','FB','BRK-B','V','JNJ','WMT', 'UNH', 'DIS', 'BAC', 'PG', 'HD', 'NVDA', 'PYPL', 'INTC', 'CMCSA', 'VZ', 'KO', 'NFLX', 'ADBE', 'T', 'CSCO', 'ABT', 'NKE', 'CVX', 'PFE', 'ORCL', 'PEP', 'MRK', 'AVGO', 'CRM', 'LLY', 'ACN', 'MCD', 'WFC', 'TMUS', 'C', 'JPM','MA','MS','GS','BA','F','GM','XOM', 'PM', 'SBUX', 'AMD', 'ABNB', 'GRMN','SPOT','TWTR','BLK','GE')

df_stks <- BatchGetSymbols::BatchGetSymbols(tickers = tickers,
                                       first.date = start_date,
                                       last.date = end_date,
                                       freq.data = 'daily',
                                       thresh.bad.data = 0.25,
                                       do.cache = F)

ggplot() +
  geom_line(data = df_indx$df.tickers, mapping = aes(x = ref.date, y = price.adjusted, color = ticker), size = 1) +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, scales = 'free_y', ncol = 1)

ggplot() +
  geom_line(data = df_indx_short$df.tickers, mapping = aes(x = ref.date, y = price.adjusted, color = ticker), size = 1) +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, scales = 'free_y', ncol = 1)

ggplot() +
  geom_line(data = df_stks$df.tickers, mapping = aes(x = ref.date, y = price.adjusted, color = ticker), size =1) +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, scales = 'free_y', ncol = 6)

df_indx$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::mutate(ln_diff_sum = rollsum(ln_diff, 260, align = "right", fill = NA)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, ncol = 1)

df_indx_short$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::mutate(ln_diff_sum = rollsum(ln_diff, 260, align = "right", fill = NA)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, ncol = 1)

df_stks$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::mutate(ln_diff_sum = rollsum(ln_diff, 260, align = "right", fill = NA)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, ncol = 6)

df_stks$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::mutate(ln_diff_sum = rollsum(ln_diff, 260, align = "right", fill = NA)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, scales = 'free_y', ncol = 6)

