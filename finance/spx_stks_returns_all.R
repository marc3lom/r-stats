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
library(stringr)
library(stringi)
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
v_f_alpha <- v_c_alpha * 0.250

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

n_long <- 6 # in years
n_short <- 1 # in years

start_date <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_long), unit = 'week')
end_date  <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(0), unit = 'week')

df_spx <- BatchGetSymbols::BatchGetSymbols(tickers = '^GSPC',
                                           first.date = start_date,
                                           last.date = end_date,
                                           freq.data = 'daily',
                                           type.return = 'log',
                                           do.cache = FALSE)
df_spx <- df_spx$df.tickers %>%
  subset(select = c(ref.date,price.adjusted,ret.adjusted.prices)) %>%
  na.omit() %>% dplyr::as_tibble()
colnames(df_spx) <- c('date','adjusted','ret_adjusted')
df_spx$date <- lubridate::ymd(df_spx$date)

df_spx_returns <- df_spx %>%
  dplyr::select(date,ret_adjusted) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(ret_adjusted))

df_spx_returns_s <- df_spx %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::select(date,ret_adjusted) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(ret_adjusted))

tickers <- tq_index('SP500') %>% as_tibble()
tickers$symbol <- stringr::str_replace_all(tickers$symbol, '\\.', '-')

shares <- tickers %>% subset(select = c(symbol,shares_held)) %>% as_tibble()
colnames(shares) <- c('ticker','shares')

df_wght_dec <- tickers %>%
  dplyr::arrange(desc(weight))

df_wght_inc <- tickers %>%
  dplyr::arrange(weight)

top_20 <- dplyr::as_tibble(df_wght_dec$symbol[1:20])
top_100 <- dplyr::as_tibble(df_wght_dec$symbol[1:100])
mid_300 <- dplyr::as_tibble(df_wght_dec$symbol[101:(nrow(df_wght_dec) - 100)])
bot_100 <- dplyr::as_tibble(df_wght_inc$symbol[1:100])

# top-100 stocks
df_t100 <- BatchGetSymbols::BatchGetSymbols(tickers = as.vector(top_100$value),
                                            first.date = start_date,
                                            last.date = end_date,
                                            freq.data = 'daily',
                                            type.return = 'log',
                                            do.cache = FALSE)

df_px_t100 <- df_t100$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted,ret.adjusted.prices) %>% na.omit() %>%
  as_tibble()

colnames(df_px_t100) <- c('date','ticker','adjusted','ret_adjusted')
df_px_t100$date <- lubridate::ymd(df_px_t100$date)
df_px_t100 <- dplyr::inner_join(df_px_t100,shares,by='ticker')

df_rets_t100 <- df_px_t100 %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_t100 <- df_px_t100 %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_t100 <- df_mcap_t100 %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_t100[1,ncol(df_sum_mcap_t100)] <- 0
df_sum_mcap_t100 <- df_sum_mcap_t100 %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# top-100 stocks (short series)
df_rets_t100_s <- df_px_t100 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_t100_s <- df_px_t100 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_t100_s <- df_mcap_t100 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_t100_s[1,ncol(df_sum_mcap_t100_s)] <- 0
df_sum_mcap_t100_s <- df_sum_mcap_t100_s %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# middle-300 stocks
df_m300 <- BatchGetSymbols::BatchGetSymbols(tickers = as.vector(mid_300$value),
                                            first.date = start_date,
                                            last.date = end_date,
                                            freq.data = 'daily',
                                            type.return = 'log',
                                            do.cache = FALSE)

df_px_m300 <- df_m300$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted,ret.adjusted.prices) %>% na.omit() %>%
  as_tibble()

colnames(df_px_m300) <- c('date','ticker','adjusted','ret_adjusted')
df_px_m300$date <- lubridate::ymd(df_px_m300$date)
df_px_m300 <- dplyr::inner_join(df_px_m300,shares,by='ticker')

df_rets_m300 <- df_px_m300 %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_m300 <- df_px_m300 %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_m300 <- df_mcap_m300 %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_m300[1,ncol(df_sum_mcap_m300)] <- 0
df_sum_mcap_m300 <- df_sum_mcap_m300 %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# middle-300 stocks (short series)
df_rets_m300_s <- df_px_m300 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_m300_s <- df_px_m300 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_m300_s <- df_mcap_m300 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_m300_s[1,ncol(df_sum_mcap_m300_s)] <- 0
df_sum_mcap_m300_s <- df_sum_mcap_m300_s %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# bottom-100 stocks
df_b100 <- BatchGetSymbols::BatchGetSymbols(tickers = as.vector(bot_100$value),
                                            first.date = start_date,
                                            last.date = end_date,
                                            freq.data = 'daily',
                                            type.return = 'log',
                                            do.cache = FALSE)

df_px_b100 <- df_b100$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted,ret.adjusted.prices) %>% na.omit() %>%
  as_tibble()

colnames(df_px_b100) <- c('date','ticker','adjusted','ret_adjusted')
df_px_b100$date <- lubridate::ymd(df_px_b100$date)
df_px_b100 <- dplyr::inner_join(df_px_b100,shares,by='ticker')

df_rets_b100 <- df_px_b100 %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_b100 <- df_px_b100 %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_b100 <- df_mcap_b100 %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_b100[1,ncol(df_sum_mcap_b100)] <- 0
df_sum_mcap_b100 <- df_sum_mcap_b100 %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# bottom-100 stocks (short series)
df_rets_b100_s <- df_px_b100 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_b100_s <- df_px_b100 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_b100_s <- df_mcap_b100 %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_b100_s[1,ncol(df_sum_mcap_b100_s)] <- 0
df_sum_mcap_b100_s <- df_sum_mcap_b100_s %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# all stocks
df_stocks <- rbind(df_px_t100,df_px_m300,df_px_b100)

df_returns <- df_stocks %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_marketcap <- df_stocks %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

# all stocks (short series)
df_returns_s <- df_stocks %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_marketcap_s <- df_stocks %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

## ggplots
p0 <- df_rets_t100 %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.15625) +
  geom_line(data = df_spx_returns, mapping = aes(x = date, y = return), size = 0.90625, color = 'black') +
  geom_line(data = df_sum_mcap_t100, mapping = aes(x = date, y = return), size = 1.09375, color = 'orangered') +
  geom_text(data = filter(df_spx_returns, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(10)), y = return), size = 4.00, fontface = 'bold') +
  geom_text(data = filter(df_sum_mcap_t100, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(10)), y = return), size = 4.00, fontface = 'bold', color = 'orangered') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: Top-100 Stocks', subtitle = 'Retorno Acumulado (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p1 <- df_rets_m300 %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.15625) +
  geom_line(data = df_spx_returns, mapping = aes(x = date, y = return), size = 0.90625, color = 'black') +
  geom_line(data = df_sum_mcap_m300, mapping = aes(x = date, y = return), size = 1.09375, color = 'orangered') +
  geom_text(data = filter(df_spx_returns, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(10)), y = return), size = 4.00, fontface = 'bold') +
  geom_text(data = filter(df_sum_mcap_m300, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(10)), y = return), size = 4.00, fontface = 'bold', color = 'orangered') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: Middle-300 Stocks', subtitle = 'Retorno Acumulado (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2 <- df_rets_b100 %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.15625) +
  geom_line(data = df_spx_returns, mapping = aes(x = date, y = return), size = 0.90625, color = 'black') +
  geom_line(data = df_sum_mcap_b100, mapping = aes(x = date, y = return), size = 1.09375, color = 'orangered') +
  geom_text(data = filter(df_spx_returns, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(10)), y = return), size = 4.00, fontface = 'bold') +
  geom_text(data = filter(df_sum_mcap_b100, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(10)), y = return), size = 4.00, fontface = 'bold', color = 'orangered') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: Bottom-100 Stocks', subtitle = 'Retorno Acumulado (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p3 <- df_returns %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.125) +
  geom_line(data = df_spx_returns, mapping = aes(x = date, y = return), size = 1.0000, color = 'black') +
  geom_text(data = filter(df_spx_returns, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(10)), y = return), size = 4.25, fontface = 'bold') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: All Stocks', subtitle = 'Retorno Acumulado (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p10 <- df_mcap_t100 %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.1875) +
  labs(title = 'S&P 500: Top-100 Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p11 <- df_mcap_m300 %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.1875) +
  labs(title = 'S&P 500: Middle-300 Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p12 <- df_mcap_b100 %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.1875) +
  labs(title = 'S&P 500: Bottom-100 Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(date_breaks = '26 weeks', date_labels = "%d %b") +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p13 <- df_marketcap %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.125) +
  labs(title = 'S&P 500: All Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p20 <- df_mcap_t100 %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: Top-100 Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p21 <- df_mcap_m300 %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: Middle-300 Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p22 <- df_mcap_b100 %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: Bottom-100 Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p23 <- df_marketcap %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: All Stocks', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

## ggplots (short series)
p4 <- df_rets_t100_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.15625) +
  geom_line(data = df_spx_returns_s, mapping = aes(x = date, y = return), size = 0.90625, color = 'black') +
  geom_line(data = df_sum_mcap_t100_s, mapping = aes(x = date, y = return), size = 1.09375, color = 'orangered') +
  geom_text(data = filter(df_spx_returns_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.00, fontface = 'bold') +
  geom_text(data = filter(df_sum_mcap_t100_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.00, fontface = 'bold', color = 'orangered') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: Top-100 Stocks', subtitle = 'Retorno Acumulado (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p5 <- df_rets_m300_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.15625) +
  geom_line(data = df_spx_returns_s, mapping = aes(x = date, y = return), size = 0.90625, color = 'black') +
  geom_line(data = df_sum_mcap_m300_s, mapping = aes(x = date, y = return), size = 1.09375, color = 'orangered') +
  geom_text(data = filter(df_spx_returns_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.00, fontface = 'bold') +
  geom_text(data = filter(df_sum_mcap_m300_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.00, fontface = 'bold', color = 'orangered') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: Middle-300 Stocks', subtitle = 'Retorno Acumulado (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p6 <- df_rets_b100_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.15625) +
  geom_line(data = df_spx_returns_s, mapping = aes(x = date, y = return), size = 0.90625, color = 'black') +
  geom_line(data = df_sum_mcap_b100_s, mapping = aes(x = date, y = return), size = 1.09375, color = 'orangered') +
  geom_text(data = filter(df_spx_returns_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.00, fontface = 'bold') +
  geom_text(data = filter(df_sum_mcap_b100_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.00, fontface = 'bold', color = 'orangered') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: Bottom-100 Stocks', subtitle = 'Retorno Acumulado (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p7 <- df_returns_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = 0.125) +
  geom_line(data = df_spx_returns_s, mapping = aes(x = date, y = return), size = 1.0000, color = 'black') +
  geom_text(data = filter(df_spx_returns_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date() + weeks(2)), y = return), size = 4.25, fontface = 'bold') +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = 'S&P 500: All Stocks', subtitle = 'Retorno Acumulado (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p14 <- df_mcap_t100_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.1875) +
  labs(title = 'S&P 500: Top-100 Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p15 <- df_mcap_m300_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.1875) +
  labs(title = 'S&P 500: Middle-300 Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p16 <- df_mcap_b100_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.1875) +
  labs(title = 'S&P 500: Bottom-100 Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(date_breaks = '26 weeks', date_labels = "%d %b") +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p17 <- df_marketcap_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = 0.125) +
  labs(title = 'S&P 500: All Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p24 <- df_mcap_t100_s %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: Top-100 Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p25 <- df_mcap_m300_s %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: Middle-300 Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p26 <- df_mcap_b100_s %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: Bottom-100 Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p27 <- df_marketcap_s %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack()) +
  labs(title = 'S&P 500: All Stocks', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p30 <- df_marketcap %>%
  dplyr::filter(ticker == pull(top_20)) %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = (1/32) * 44) +
  labs(title = 'S&P 500: Top-20', subtitle = 'Market Cap (6 Anos)', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p31 <- df_marketcap_s %>%
  dplyr::filter(ticker == pull(top_20)) %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = (1/32) * 44) +
  labs(title = 'S&P 500: Top-20', subtitle = 'Market Cap (1 Ano)', caption = NULL, x = NULL, y = NULL) +
  theme(legend.position = 'bottom') +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

grid.arrange(p0,p4,ncol = 1)
grid.arrange(p1,p5,ncol = 1)
grid.arrange(p2,p6,ncol = 1)
grid.arrange(p3,p7,ncol = 1)

grid.arrange(p10,p14,ncol = 1)
grid.arrange(p11,p15,ncol = 1)
grid.arrange(p12,p16,ncol = 1)
grid.arrange(p13,p17,ncol = 1)

# grid.arrange(p20,p24,ncol = 1)
# grid.arrange(p21,p25,ncol = 1)
# grid.arrange(p22,p26,ncol = 1)
# grid.arrange(p23,p27,ncol = 1)
