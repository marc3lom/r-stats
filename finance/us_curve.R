rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd("g:/Diest/marcelom/R/strategy/")
# setwd("d:/usr/marcelom/OneDrive/Code/R/taa/")
# setwd("/home/marcelom/.local/code/r/finance/")

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
library(gridExtra)
library(scales)
# theme_set(theme_minimal())
theme_set(theme_bw())
library(PerformanceAnalytics)
# quandl_api_key("SfzU7dizd7zAmk8crhqw")
# df_quandl <- tq_get("BCIW/_INX", get = "quandl", from = "1927-12-30")

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

v_c_alpha <- 1.000
# v_c_alpha <- 0.750
v_f_alpha <- 1.000
# v_f_alpha <- 0.500

# user defined functions

n_st <- trunc(52*5/52)
n_mt <- trunc(52*5/12)
n_lt <- trunc(52*5/4)

mav_st <- function(x,n = n_st){stats::filter(x,rep(1/n,n), sides=1)}
mav_mt <- function(x,n = n_mt){stats::filter(x,rep(1/n,n), sides=1)}
mav_lt <- function(x,n = n_lt){stats::filter(x,rep(1/n,n), sides=1)}

mav_st_b <- function(x,n = 2*n_st+1){stats::filter(x,rep(1/n,n), sides=2)}
mav_mt_b <- function(x,n = 2*n_mt+1){stats::filter(x,rep(1/n,n), sides=2)}
mav_lt_b <- function(x,n = 2*n_lt+1){stats::filter(x,rep(1/n,n), sides=2)}

gm <- function(x){exp(mean(log(x[is.finite(log(x))])))}

s_rets <- function(x){
  len <- nrow(x)
  xDif <- x[2:len,] / x[1:len-1,] - 1
}

g_rets <- function(x){
  len <- nrow(x)
  xDif <- x[2:len,] / x[1:len-1,] 
}

l_rets <- function(x){
  len <- nrow(x)
  xDif <- log(x[2:len,]) - log(x[1:len-1,] )
}

# leitura de dados

# by spreadsheet

# df <- read_xlsx("d:/usr/marcelom/OneDrive/Code/R/taa/curve_trade.xlsx", sheet = "dataRead") # dados mensais
# df$date <- lubridate::ymd(df$date)

# by Bloomberg
# Connect to Bloomberg
library(Rblpapi)
blpConnect()

# Assign tickers and fields
df_tickers <- data.frame(c('FED5YEAR Index','USGG5Y5Y Index','FEDL01 Index',
                           'USGG2YR Index', 'USGG5YR Index', 'USGG10YR Index', 'USGG30YR Index'))
colnames(df_tickers) <- 'ticker'

# bbg_field <- 'CUR_MKT_CAP'
bbg_field_opt <- 'TICKER'
bbg_field <- 'PX_LAST'

freq <- as.character('d') # alterar valor manualmente

lag_years <- 20 # alterar valor manualmente
start_date = lubridate::ceiling_date(Sys.Date() - years(lag_years), unit = 'week')
end_date = as.Date(Sys.Date())

per_bbg <- ifelse(freq == "d", as.character("DAILY"), ifelse(freq == "w", as.character("WEEKLY"), ifelse(freq == "m", as.character("MONTHLY"), as.character("QUARTERLY"))))
# pts_bbg <- ifelse(freq == "d", 52*5, ifelse(freq == "w", 52, ifelse(freq == "m", 12, 4))) * lag_years
opt_bbg <- c("nonTradingDayFillOption"="NON_TRADING_WEEKDAYS",
             "nonTradingDayFillMethod"="PREVIOUS_VALUE",
             "periodicitySelection"=per_bbg)#,"maxDataPoints"=pts_bbg)

# Pull Bloomberg data and create data frame

for (i in 1:nrow(df_tickers)) {
  x <- as.data.frame(bdh(df_tickers[i,1],
                         bbg_field,
                         start.date = start_date,
                         end.date   = end_date,
                         options = opt_bbg))
  colnames(x) <- c('date',bdp(df_tickers[i,1],bbg_field_opt)) %>% tolower()
  name_1 <- paste('df',i,sep = '_')
  assign(name_1,x)
  rm(x)
}

df <- df_1 %>% merge(df_2,by='date') %>% merge(df_3,by='date') %>% 
  merge(df_4,by='date') %>% merge(df_5,by='date') %>% merge(df_6,by='date') %>% merge(df_7,by='date')

df$date <- lubridate::ymd(df$date)

# preparando dados, calculando retornos

nr <- nrow(df)
nc <- ncol(df)

df %>%
  dplyr::select(date,fedl01,usgg2yr,usgg5yr,usgg10yr,usgg30yr,usgg5y5y) %>%
  mutate(us_on = fedl01 * 100) %>%
  mutate(us_02y = usgg2yr * 100) %>%
  mutate(us_05y = usgg5yr * 100) %>%
  mutate(us_10y = usgg10yr * 100) %>%
  mutate(us_30y = usgg30yr * 100) %>%
  mutate(us_5y5y = usgg5y5y * 100) %>%
  dplyr::select(date,us_on,us_02y,us_05y,us_10y,us_30y,us_5y5y) %>%
  dplyr::arrange(date) %>%
  pivot_longer(cols =-date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ticker, ncol = 2, scales = 'free_y')

df %>%
  dplyr::select(date,usgg2yr,usgg5yr,usgg10yr,usgg30yr) %>%
  mutate(curve_02_05 = (usgg5yr - usgg2yr) * 100) %>%
  mutate(curve_02_10 = (usgg10yr - usgg2yr) * 100) %>%
  mutate(curve_02_30 = (usgg30yr - usgg2yr) * 100) %>%
  mutate(curve_05_10 = (usgg10yr - usgg5yr) * 100) %>%
  mutate(curve_05_30 = (usgg30yr - usgg5yr) * 100) %>%
  mutate(curve_10_30 = (usgg30yr - usgg10yr) * 100) %>%
  dplyr::select(date,curve_02_05,curve_02_10,curve_02_30,curve_05_10,curve_05_30,curve_10_30) %>%
  dplyr::arrange(date) %>%
  pivot_longer(cols =-date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ticker, ncol = 2, scales = 'free_y')

