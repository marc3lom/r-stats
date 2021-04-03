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
library(gridExtra)
library(scales)

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

df <- read_xlsx("c:/Hvymtl/usr/R/code/taa/curve_trade.xlsx", sheet = "dataRead") # dados mensais
df$date <- lubridate::ymd(df$date)
nr <- nrow(df)
nc <- ncol(df)

# preparando dados, calculando retornos

df %>%
  dplyr::select(date,fedl01,usgg2yr,usgg5yr,usgg10yr,usgg30yr) %>%
  dplyr::arrange(date) %>%
  pivot_longer(cols =-date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1) +
  scale_color_viridis_d(option = 'E') +
  scale_fill_viridis_d(option = 'E', alpha = 0.50)

df %>%
  dplyr::select(date,usgg2yr,usgg5yr,usgg10yr,usgg30yr) %>%
  mutate(fly_2_5_10 = (2 * usgg5yr - (usgg2yr + usgg10yr)) * 100) %>%
  mutate(fly_5_10_30 = (2 * usgg10yr - (usgg30yr + usgg5yr)) * 100) %>%
  dplyr::select(date,fly_2_5_10,fly_5_10_30) %>%
  dplyr::arrange(date) %>%
  pivot_longer(cols =-date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1) +
  scale_color_viridis_d(option = 'E') +
  scale_fill_viridis_d(option = 'E', alpha = 0.50)

