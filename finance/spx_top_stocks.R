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
v_f_alpha <- v_c_alpha * (1/32) * 21

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

n_long <- 8 # in years
n_short <- 1 # in years

freq <- 'daily'

start_date <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_long), unit = 'week')
end_date  <- lubridate::ceiling_date(Sys.Date() - lubridate::weeks(0), unit = 'week')

df_spx <- BatchGetSymbols::BatchGetSymbols(tickers = '^GSPC',
                                           first.date = start_date,
                                           last.date = end_date,
                                           freq.data = freq,
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

df_wght_top <- tickers %>%
  dplyr::arrange(desc(weight))

n_stocks <- 25

top_stocks <- dplyr::as_tibble(df_wght_top$symbol[1:n_stocks])
bot_stocks <- dplyr::as_tibble(df_wght_top$symbol[(n_stocks+1):nrow(df_wght_top)])

# top stocks
df_top <- BatchGetSymbols::BatchGetSymbols(tickers = as.vector(top_stocks$value),
                                           first.date = start_date,
                                           last.date = end_date,
                                           freq.data = freq,
                                           type.return = 'log',
                                           do.cache = FALSE)

df_px_top <- df_top$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted,ret.adjusted.prices) %>% na.omit() %>%
  as_tibble()

colnames(df_px_top) <- c('date','ticker','adjusted','ret_adjusted')
df_px_top$date <- lubridate::ymd(df_px_top$date)
df_px_top <- dplyr::inner_join(df_px_top,shares,by='ticker')

df_rets_top <- df_px_top %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_top <- df_px_top %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_top <- df_mcap_top %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_top[1,ncol(df_sum_mcap_top)] <- 0
df_sum_mcap_top <- df_sum_mcap_top %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# top stocks (short series)
df_rets_top_s <- df_px_top %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_top_s <- df_px_top %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_top_s <- df_mcap_top %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_top_s[1,ncol(df_sum_mcap_top_s)] <- 0
df_sum_mcap_top_s <- df_sum_mcap_top_s %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# bottom stocks
df_bot <- BatchGetSymbols::BatchGetSymbols(tickers = as.vector(bot_stocks$value),
                                           first.date = start_date,
                                           last.date = end_date,
                                           freq.data = freq,
                                           type.return = 'log',
                                           do.cache = FALSE)

df_px_bot <- df_bot$df.tickers %>%
  dplyr::select(ref.date,ticker,price.adjusted,ret.adjusted.prices) %>% na.omit() %>%
  as_tibble()

colnames(df_px_bot) <- c('date','ticker','adjusted','ret_adjusted')
df_px_bot$date <- lubridate::ymd(df_px_bot$date)
df_px_bot <- dplyr::inner_join(df_px_bot,shares,by='ticker')

df_rets_bot <- df_px_bot %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_bot <- df_px_bot %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_bot <- df_mcap_bot %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_bot[1,ncol(df_sum_mcap_bot)] <- 0
df_sum_mcap_bot <- df_sum_mcap_bot %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

# top stocks (short series)
df_rets_bot_s <- df_px_bot %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(return = exp(cumsum(ret_adjusted)) - 1) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_mcap_bot_s <- df_px_bot %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::arrange(date) %>% dplyr::group_by(ticker) %>%
  dplyr::mutate(mkt_cap = (adjusted * shares) / 10000) %>%
  dplyr::select(-c(adjusted,ret_adjusted,shares))

df_sum_mcap_bot_s <- df_mcap_bot %>%
  dplyr::filter(date >= lubridate::ceiling_date(Sys.Date() - lubridate::weeks(52 * n_short))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(agg_mcap = sum(mkt_cap)) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(return = TTR::ROC(agg_mcap))
df_sum_mcap_bot_s[1,ncol(df_sum_mcap_bot_s)] <- 0
df_sum_mcap_bot_s <- df_sum_mcap_bot_s %>%
  dplyr::mutate(return = exp(cumsum(return)) - 1)

## market cap calculations
df_mktcap_all <- cbind(df_sum_mcap_top,df_sum_mcap_bot)
colnames(df_mktcap_all) <- c('date','mktcap_top','ret_top','dt','mktcap_bot','ret_bot')
df_mktcap_all <- df_mktcap_all %>%
  dplyr::select(date,mktcap_top,mktcap_bot,ret_top,ret_bot) %>%
  dplyr::mutate(mktcap_all = mktcap_top + mktcap_bot) %>%
  dplyr::mutate(ret_all = TTR::ROC(mktcap_all)) %>%
  dplyr::select(date,mktcap_top,mktcap_bot,mktcap_all,ret_top,ret_bot,ret_all) %>%
  dplyr::arrange(date)
df_mktcap_all[1,ncol(df_mktcap_all)] <- 0
df_mktcap_all <- df_mktcap_all %>%
  dplyr::mutate(top_share = mktcap_top / mktcap_all) %>%
  dplyr::mutate(bot_share = 1 - top_share)

## ggplots
p0 <- df_rets_top %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = (1/32)*17) +
  geom_line(data = df_spx_returns, mapping = aes(x = date, y = return), size = (1/32)*37, color = 'black') +
  geom_line(data = df_sum_mcap_top, mapping = aes(x = date, y = return), size = (1/32)*41, color = 'orangered') +
  geom_text(data = filter(df_spx_returns, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date()), y = return), size = 4.00, fontface = 'bold', nudge_x = +120) +
  geom_text(data = filter(df_sum_mcap_top, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date()), y = return), size = 4.00, fontface = 'bold', color = 'orangered', nudge_x = +120) +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = paste('S&P500: Top-',n_stocks,' Stocks',sep = ''), subtitle = paste('Retorno Acumulado: ',n_long*52,' Semanas',sep = ''), caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p1 <- df_mcap_top %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = (1/32)*17) +
  labs(title = paste('S&P500: Top-',n_stocks,' Stocks',sep = ''), subtitle = paste('Market Cap: ',n_long*52,' Semanas',sep = ''), caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2 <- df_mcap_top %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack(), color = 'white') +
  labs(title = paste('S&P500: Top-',n_stocks,' Stocks',sep = ''), subtitle = paste('Market Cap: ',n_long*52,' Semanas',sep = ''), caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

## ggplots (short series)
p10 <- df_rets_top_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = return, color = ticker, group = ticker), size = (1/32)*17) +
  geom_line(data = df_spx_returns_s, mapping = aes(x = date, y = return), size = (1/32)*37, color = 'black') +
  geom_line(data = df_sum_mcap_top_s, mapping = aes(x = date, y = return), size = (1/32)*41, color = 'orangered') +
  geom_text(data = filter(df_spx_returns_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date()), y = return), size = 4.00, fontface = 'bold', nudge_x = +14, nudge_y = -0.01) +
  geom_text(data = filter(df_sum_mcap_top_s, date == tail(date, n = 1)), aes(label = percent(return, accuracy = 0.01), x = as.Date(Sys.Date()), y = return), size = 4.00, fontface = 'bold', color = 'orangered', nudge_x = +14, nudge_y = +0.01) +
  geom_hline(yintercept = 0, size = 0.75, color = "gray32", alpha = v_f_alpha) +
  labs(title = paste('S&P500: Top-',n_stocks,' Stocks',sep = ''), subtitle = paste('Retorno Acumulado: ',n_short*52,' Semanas',sep = ''), caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p11 <- df_mcap_top_s %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker), size = (1/32)*17) +
  labs(title = paste('S&P500: Top-',n_stocks,' Stocks',sep = ''), subtitle = paste('Market Cap: ',n_short*52,' Semanas',sep = ''), caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p12 <- df_mcap_top_s %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = mkt_cap, color = ticker, group = ticker, fill = ticker), position = position_stack(), color = 'white') +
  labs(title = paste('S&P500: Top-',n_stocks,' Stocks',sep = ''), subtitle = paste('Market Cap: ',n_short*52,' Semanas',sep = ''), caption = NULL, x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# plot grids

grid.arrange(p0,p10,ncol = 1)
grid.arrange(p1,p11,ncol = 1)
grid.arrange(p2,p12,ncol = 1)

# Mkt share

df_mktcap_all %>%
  dplyr::select(date,top_share,bot_share) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = values, color = values, group = ticker), size = (1/32)*37) +
  geom_text(data = filter(df_mktcap_all, date == tail(date, n = 1)), aes(label = percent(top_share, accuracy = 0.01), x = as.Date(Sys.Date()), y = top_share), size = 4.00, fontface = 'bold', nudge_x = +120) +
  geom_text(data = filter(df_mktcap_all, date == tail(date, n = 1)), aes(label = percent(bot_share, accuracy = 0.01), x = as.Date(Sys.Date()), y = bot_share), size = 4.00, fontface = 'bold', nudge_x = +120) +
  annotate('text', x = floor_date(tail(df_mktcap_all$date,1), unit = 'years'), y = .3, label = paste(n_stocks,'Maiores Empresas',sep = '\n'),size = 4.00, fontface = 'bold') + 
  annotate('text', x = floor_date(tail(df_mktcap_all$date,1), unit = 'years'), y = .7, label = paste(nrow(df_wght_top)-n_stocks,'Menores Empresas',sep = '\n'), size = 4.00, fontface = 'bold') + 
  labs(title = paste('S&P500: Top-',n_stocks,' Stocks',sep = ''), subtitle = 'Market Cap % Total', caption = NULL, x = NULL, y = NULL) +
  scale_x_date() +
  scale_y_percent(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = FALSE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = FALSE)


