### initial settings

rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/covid/')
setwd('d:/usr/marcelom/OneDrive/Code/R/covid/')
# setwd('/home/marcelom/.local/code/r/covid/')

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

library(dotenv)
load_dot_env(file = 'd:/usr/marcelom/OneDrive/Code/R/.env')
# load_dot_env(file = '/home/marcelom/.env')
# calling .env variable example
# quandl_api_key(Sys.getenv('quandl_key'))

### ggplot theme settings

# theme_set(theme_bw())
theme_set(theme_minimal())

# Sys.setlocale("LC_ALL", "Portuguese")
Sys.setlocale("LC_TIME", "Portuguese")

# viridis options
library(viridisLite)
library(viridis)

# v_color <- as.character('A') # magma
# v_color <- as.character('B') # inferno
# v_color <- as.character('C') # plasma
# v_color <- as.character('D') # viridis
v_color <- as.character('E') # cividis
# v_color <- as.character('F') # rocket
# v_color <- as.character('G') # mako
# v_color <- as.character('H') # turbo

v_c_alpha <- 1.000
# v_c_alpha <- 0.500
v_f_alpha <- v_c_alpha / 2

### custom functions

n_st <- 7
n_mt <- 14
n_lt <- 28

mav_st <- function(x,n = n_st){stats::filter(x,rep(1/n,n), sides=1)}
mav_mt <- function(x,n = n_mt){stats::filter(x,rep(1/n,n), sides=1)}
mav_lt <- function(x,n = n_lt){stats::filter(x,rep(1/n,n), sides=1)}

mav_st_b <- function(x,n = 2*n_st+1){stats::filter(x,rep(1/n,n), sides=2)}
mav_mt_b <- function(x,n = 2*n_mt+1){stats::filter(x,rep(1/n,n), sides=2)}
mav_lt_b <- function(x,n = 2*n_lt+1){stats::filter(x,rep(1/n,n), sides=2)}

mav_st_d <- function(x,n = n_st){forecast::ma(x, n, centre = FALSE)}

gm_mean <- function(x){exp(mean(log(x[is.finite(log(x))])))}

### getting and preparing data

df_nyt <- read.csv("https://github.com/nytimes/covid-19-data/raw/master/us.csv", stringsAsFactors = FALSE)
df_nyt$date <- lubridate::ymd(df_nyt$date)

start_date <- as.Date(head(df_nyt$date, n = 1))
end_date <- lubridate::ceiling_date(Sys.Date() + lubridate::weeks(2))

# data mods

df_nyt <- df_nyt %>%
  dplyr::arrange(date) %>%
  mutate(cases_1d_delta = cases - lag(cases, n = 1)) %>%
  mutate(deaths_1d_delta = deaths - lag(deaths, n = 1)) %>%
  mutate(cases_7d_mavg = mav_st(cases_1d_delta)) %>%
  mutate(deaths_7d_mavg = mav_st(deaths_1d_delta))

### GGPlots

p1 <- df_nyt %>%
  dplyr::select(date,cases) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = cases, color = cases, fill = cases), position="stack", stat="identity") +
  labs(title = "EUA: COVID-19", subtitle = "Casos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p2 <- df_nyt %>%
  dplyr::select(date,deaths) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = deaths, color = deaths, fill = deaths), position="stack", stat="identity") +
  labs(title = "EUA: COVID-19", subtitle = "Obitos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p3 <- df_nyt %>%
  dplyr::select(date,cases_1d_delta,cases_7d_mavg) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = cases_1d_delta, color = cases_1d_delta, fill = cases_1d_delta), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(x = date, y = cases_7d_mavg), color = 'gray16', size = 1.25) +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(cases_7d_mavg, accuracy = 1), x = date, y = cases_7d_mavg), hjust = -.25, position = position_dodge(.9), size = 4.25, fontface = "bold") +
  labs(title = "EUA: COVID-19", subtitle = "Casos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = "8 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p4 <- df_nyt %>%
  dplyr::select(date,deaths_1d_delta,deaths_7d_mavg) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = deaths_1d_delta, color = deaths_1d_delta, fill = deaths_1d_delta), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(x = date, y = deaths_7d_mavg), color = 'gray16', size = 1.25) +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(deaths_7d_mavg, accuracy = 1), x = date, y = deaths_7d_mavg), hjust = -.25, position = position_dodge(.9), size = 4.25, fontface = "bold") +
  labs(title = "EUA: COVID-19", subtitle = "Obitos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = "8 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

## plot grids

grid.arrange(p1,p2,nrow=2,ncol=1)
grid.arrange(p3,p4,nrow=2,ncol=1)

g1 <- list(p1,p2,p3,p4)

grid.arrange(grobs = g1,
             widths = c(1,1,1),
             layout_matrix = rbind(c(1,3,3),
                                   c(4,4,2)))

