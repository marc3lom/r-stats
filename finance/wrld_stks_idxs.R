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

lag_years <- 1

start_date <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks((52*lag_years)+1), unit = 'week')
end_date  <- Sys.Date()
chart_date <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52*lag_years), unit = 'week')
trade_date <- lubridate::ceiling_date(as.Date('2020-09-24') - lubridate::weeks(-1), unit = 'week')

indx_all <- c('^GSPC','^IXIC', '^STOXX50E', '^FTSE', '^N225', '^AORD', '^GSPTSE', '^BVSP')
indx_less <- c('^GSPC','^IXIC', '^STOXX50E', '^FTSE')

df_indx_all <- BatchGetSymbols::BatchGetSymbols(tickers = indx_all,
                                            first.date = start_date,
                                            last.date = end_date,
                                            freq.data = 'daily',
                                            thresh.bad.data = 0.60,
                                            do.cache = F)

df_indx_less <- BatchGetSymbols::BatchGetSymbols(tickers = indx_less,
                                            first.date = start_date,
                                            last.date = end_date,
                                            freq.data = 'daily',
                                            thresh.bad.data = 0.60,
                                            do.cache = F)

ggplot() +
  geom_line(data = df_indx_all$df.tickers, mapping = aes(x = ref.date, y = price.adjusted, color = ticker), size = 1) +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, scales = 'free_y', ncol = 2)

df_indx_all$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::filter(ref.date >= chart_date) %>%
  dplyr::mutate(ln_diff_sum = cumsum(ln_diff)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, ncol = 2)

df_indx_all$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::filter(ref.date >= chart_date) %>%
  dplyr::mutate(ln_diff_sum = cumsum(ln_diff)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, ncol = 2)

ggplot() +
  geom_line(data = df_indx_less$df.tickers, mapping = aes(x = ref.date, y = price.adjusted, color = ticker), size = 1) +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, scales = 'free_y', ncol = 2)

df_indx_less$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::filter(ref.date >= chart_date) %>%
  dplyr::mutate(ln_diff_sum = cumsum(ln_diff)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, ncol = 2)

df_indx_less$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::filter(ref.date >= trade_date) %>%
  dplyr::mutate(ln_diff_sum = cumsum(ln_diff)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1) +
  scale_y_percent() +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~ticker, ncol = 2)

df_indx_less$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  dplyr::mutate(ln_diff = log(price.adjusted) - lag(log(price.adjusted))) %>%
  dplyr::filter(ref.date >= trade_date) %>%
  dplyr::mutate(ln_diff_sum = cumsum(ln_diff)) %>%
  dplyr::mutate(roll_rets = exp(ln_diff_sum) - 1) %>%
  dplyr::select(ref.date,ticker,roll_rets) %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(ref.date) %>%
  ggplot() +
  geom_line(mapping = aes(x = ref.date, y = roll_rets, color = ticker), size = 1.25) +
  scale_y_percent() +
  theme(legend.position = 'right') +
  scale_color_viridis_d(option = "E") +
  scale_fill_viridis_d(option = "E")
