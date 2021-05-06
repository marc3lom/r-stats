### initial settings

rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/covid/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/covid/')
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

df_jhu_full <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv', stringsAsFactors = F)
df_jhu_full$date <- lubridate::ymd(df_jhu_full$date)
df_jhu_cases <- read.csv('https://github.com/owid/covid-19-data/raw/master/public/data/jhu/new_cases_per_million.csv', stringsAsFactors = F)
df_jhu_cases$date <- lubridate::ymd(df_jhu_cases$date)
df_jhu_cases_new <- read.csv('https://github.com/owid/covid-19-data/raw/master/public/data/jhu/new_cases.csv', stringsAsFactors = F)
df_jhu_cases_new$date <- lubridate::ymd(df_jhu_cases_new$date)
df_jhu_deaths <- read.csv('https://github.com/owid/covid-19-data/raw/master/public/data/jhu/new_deaths_per_million.csv', stringsAsFactors = F)
df_jhu_deaths$date <- lubridate::ymd(df_jhu_deaths$date)
df_jhu_deaths_new <- read.csv('https://github.com/owid/covid-19-data/raw/master/public/data/jhu/new_deaths.csv', stringsAsFactors = F)
df_jhu_deaths_new$date <- lubridate::ymd(df_jhu_deaths_new$date)

start_date <- as.Date(head(df_jhu_full$date, n = 1))
end_date <- lubridate::ceiling_date(Sys.Date() + lubridate::weeks(2))

### GGPlots

### John Hopkins University

ctry_names_jhu <- c('Italy', 'Germany', 'United.States', 'United.Kingdom', 'Brazil', 'France', 'Israel', 'Spain','Portugal', 'China', 'Russia', 'India')

df_jhu_cases_plot <- df_jhu_cases %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'cases') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry_names_jhu) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cases_7d = mav_st(cases))

df_jhu_deaths_plot <- df_jhu_deaths %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'deaths') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry_names_jhu) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(deaths_7d = mav_st(deaths))

df_jhu_cases_new_plot <- df_jhu_cases_new %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'cases') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry_names_jhu) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cases_7d = mav_st(cases))

df_jhu_deaths_new_plot <- df_jhu_deaths_new %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'deaths') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry_names_jhu) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(deaths_7d = mav_st(deaths))

p_jhu_1 <- ggplot() +
  geom_bar(data = df_jhu_cases_plot, mapping = aes(x = date, y = cases_7d, color = country, fill = country), position="stack", stat="identity") +
  geom_line(data = df_jhu_cases_plot, mapping = aes(x = date, y = cases_7d), size = 1.000) +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(limits = c(-250,1750), breaks = seq(0,1500,500), name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

p_jhu_2 <- ggplot() +
  geom_bar(data = df_jhu_deaths_plot, mapping = aes(x = date, y = deaths_7d, color = country, fill = country), position="stack", stat="identity") +
  geom_line(data = df_jhu_deaths_plot, mapping = aes(x = date, y = deaths_7d), size = 1.000) +
  labs(title = NULL, subtitle = "Obitos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(limits = c(-5,35), breaks = seq(0,30,10), name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

p_jhu_3 <- ggplot() +
  geom_bar(data = df_jhu_cases_new_plot, mapping = aes(x = date, y = cases_7d, color = country, fill = country), position="stack", stat="identity") +
  geom_line(data = df_jhu_cases_new_plot, mapping = aes(x = date, y = cases_7d), size = 1.000) +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

p_jhu_4 <- ggplot() +
  geom_bar(data = df_jhu_deaths_new_plot, mapping = aes(x = date, y = deaths_7d, color = country, fill = country), position="stack", stat="identity") +
  geom_line(data = df_jhu_deaths_new_plot, mapping = aes(x = date, y = deaths_7d), size = 1.000) +
  labs(title = "COVID-19", subtitle = "Obitos Novos por Dia (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

### John Hopkins University por pais

ctry <- as.character(c('Brazil','India','World'))
n_ctry_col <- 3
n_ctry_row <- 1

p_jhu_ctry_1 <- df_jhu_cases %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'values') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(values_7d = mav_st(values)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = values, color = values, fill = values), position="stack", stat="identity") +
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.25) +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha) +
  facet_wrap(~country, ncol = n_ctry_col/n_ctry_row, nrow = n_ctry_row)

p_jhu_ctry_2 <- df_jhu_deaths %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'values') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(values_7d = mav_st(values)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = values, color = values, fill = values), position="stack", stat="identity") +
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.25) +
  labs(title = NULL, subtitle = "Obitos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha) +
  facet_wrap(~country, ncol = n_ctry_col/n_ctry_row, nrow = n_ctry_row)

p_jhu_ctry_3 <- df_jhu_cases_new %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'values') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(values_7d = mav_st(values)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = values, color = values, fill = values), position="stack", stat="identity") +
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.25) +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha) +
  facet_wrap(~country, ncol = n_ctry_col/n_ctry_row, nrow = n_ctry_row)

p_jhu_ctry_4 <- df_jhu_deaths_new %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'values') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(values_7d = mav_st(values)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = values, color = values, fill = values), position="stack", stat="identity") +
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.25) +
  labs(title = "COVID-19", subtitle = "Obitos Novos por Dia (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_y_comma(name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha) +
  facet_wrap(~country, ncol = n_ctry_col/n_ctry_row, nrow = n_ctry_row)

## Grid Plots

p_jhu_ctry_1
p_jhu_ctry_2
grid.arrange(p_jhu_ctry_1,p_jhu_ctry_2,nrow=2,ncol=1)
grid.arrange(p_jhu_ctry_3,p_jhu_ctry_4,nrow=2,ncol=1)

grid.arrange(p_jhu_1,p_jhu_2,nrow=2,ncol=1)
grid.arrange(p_jhu_3,p_jhu_4,nrow=2,ncol=1)



