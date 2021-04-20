rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/politics/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/covid')
# setwd('/home/marcelom/.local/code/r/covid/')

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
# theme_set(theme_minimal())
theme_set(theme_bw())

# viridis options
library(viridisLite)
library(viridis)

# v_color <- as.character('A') # magma
# v_color <- as.character('B') # inferno
# v_color <- as.character('C') # plasma
# v_color <- as.character('D') # viridis
# v_color <- as.character('E') # cividis
# v_color <- as.character('F') # rocket
v_color <- as.character('G') # mako
# v_color <- as.character('H') # turbo

# v_c_alpha <- 1.000
v_c_alpha <- 0.750
# v_f_alpha <- 1.000
v_f_alpha <- 0.500

# custom functions

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

# data harvest

ed <- as.Date(Sys.Date()+7)

df_nyt <- read.csv("https://github.com/nytimes/covid-19-data/raw/master/us.csv", stringsAsFactors = FALSE)
df_nyt$date <- lubridate::ymd(df_nyt$date)

df_st_nyt <- read.csv("https://github.com/nytimes/covid-19-data/raw/master/us-states.csv", stringsAsFactors = FALSE)
df_st_nyt$date <- lubridate::ymd(df_st_nyt$date)

df_rtl <- read.csv("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv", stringsAsFactors = FALSE)
df_rtl$date <- lubridate::ymd(df_rtl$date)

df_ctp <- read.csv("https://covidtracking.com/data/download/national-history.csv", stringsAsFactors = FALSE)
df_ctp$date <- lubridate::ymd(df_ctp$date)

df_who <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors = FALSE)
colnames(df_who)[1] <- 'date'
df_who$date <- lubridate::ymd(df_who$date)

df_owid <- as.data.frame(read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv', stringsAsFactors = F))
df_owid$date <- lubridate::ymd(df_owid$date)

# df_owid_vaccinations <- as.data.frame(read.csv('https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv', stringsAsFactors = F))
df_owid_vaccinations <- as.data.frame(read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv', stringsAsFactors = F))
df_owid_vaccinations$date <- lubridate::ymd(df_owid_vaccinations$date)

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

# data mods

df_nyt <- df_nyt %>%
  dplyr::arrange(date) %>%
  mutate(d_deaths = deaths - lag(deaths, n = 1)) %>%
  mutate(st_d_deaths = mav_st(d_deaths)) %>%
  mutate(mt_d_deaths = mav_mt(d_deaths)) %>%
  mutate(lt_d_deaths = mav_lt(d_deaths))

df_nyt <- df_nyt %>%
  dplyr::arrange(date) %>%
  mutate(d_cases = cases - lag(cases, n = 1)) %>%
  mutate(st_d_cases = mav_st(d_cases)) %>%
  mutate(mt_d_cases = mav_mt(d_cases)) %>%
  mutate(lt_d_cases = mav_lt(d_cases))

df_st_nyt <- df_st_nyt %>%
  dplyr::group_by(state) %>%
  dplyr::arrange(date) %>%
  mutate(d_deaths = deaths - lag(deaths, n = 1))

df_st_nyt <- df_st_nyt %>%
  dplyr::group_by(state) %>%
  dplyr::arrange(date) %>%
  mutate(d_cases = cases - lag(cases, n = 1))

df_rtl_plot <- df_rtl %>%
  dplyr::select(date, region, new_cases, new_deaths) %>%
  dplyr::arrange(region, date)

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
  geom_line(data = df_jhu_cases_plot, mapping = aes(x = date, y = cases_7d), size = 0.875) +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  scale_y_comma(limits = c(-250,1750), breaks = seq(0,1500,500), name = NULL) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b") +
  # scale_x_date(limits = c(filter(df_jhu_cases, date == head(date, n = 1))[,1],ed), date_breaks = "2 months", date_labels = "%b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

p_jhu_2 <- ggplot() +
  geom_bar(data = df_jhu_deaths_plot, mapping = aes(x = date, y = deaths_7d, color = country, fill = country), position="stack", stat="identity") +
  geom_line(data = df_jhu_deaths_plot, mapping = aes(x = date, y = deaths_7d), size = 0.875) +
  labs(title = NULL, subtitle = "Obitos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  scale_y_comma(limits = c(-5,35), breaks = seq(0,30,10), name = NULL) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b") +
  # scale_x_date(limits = c(filter(df_jhu_cases, date == head(date, n = 1))[,1],ed), date_breaks = "2 months", date_labels = "%b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

p_jhu_3 <- ggplot() +
  geom_bar(data = df_jhu_cases_new_plot, mapping = aes(x = date, y = cases_7d, color = country, fill = country), position="stack", stat="identity") +
  geom_line(data = df_jhu_cases_new_plot, mapping = aes(x = date, y = cases_7d), size = 0.875) +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  # scale_y_comma(limits = c(-50000,250000), breaks = seq(0,200000,100000), name = NULL) +
  # scale_x_date(limits = c(filter(df_jhu_cases, date == head(date, n = 1))[,1],ed), date_breaks = "4 months", date_labels = "%b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

p_jhu_4 <- ggplot() +
  geom_bar(data = df_jhu_deaths_new_plot, mapping = aes(x = date, y = deaths_7d, color = country, fill = country), position="stack", stat="identity") +
  geom_line(data = df_jhu_deaths_new_plot, mapping = aes(x = date, y = deaths_7d), size = 0.875) +
  labs(title = "COVID-19", subtitle = "Obitos Novos por Dia (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  # scale_y_comma(limits = c(-500,2500), breaks = seq(0,2000,1000), name = NULL) +
  # scale_x_date(limits = c(filter(df_jhu_cases, date == head(date, n = 1))[,1],ed), date_breaks = "4 months", date_labels = "%b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ country)

### WHO

df_who_plot <- df_who %>%
  dplyr::filter(Country_code == c('US', 'GB', 'IT', 'ES', 'FR', 'DE', 'CA', 'BR', 'SE', 'KR', 'CN', 'MX')) %>%
  dplyr::select(date, Country, New_cases, New_deaths)

p_who_1 <- ggplot() +
  geom_bar(data = df_who_plot, mapping = aes(x = date, y = New_cases, color = New_cases, fill = New_cases), position="stack", stat="identity", show.legend = TRUE) +
  # geom_line(data = df_ctp_plot, mapping = aes(x = date, y = cases_7d), size = 1.25) +
  # geom_text(data = filter(df_ctp_plot, date == tail(date, n = 1)), aes(label = comma(cases_7d, accuracy = 1), x = date, y = cases_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "Global: COVID-19", subtitle = "Casos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(limits = c(0,162500), breaks = seq(0,150000,25000), name = NULL) +
  # scale_y_comma(name = NULL) +
  scale_x_date(limits = c(filter(df_who_plot, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha) +
  facet_wrap(~ Country)

### The COVID Tracking Project

df_ctp_plot <- df_ctp %>%
  dplyr::arrange(date) %>%
  dplyr::select(date, hospitalizedCurrently, positiveIncrease, deathIncrease) %>%
  dplyr::rename(sos = hospitalizedCurrently) %>%
  dplyr::rename(cases = positiveIncrease) %>%
  dplyr::rename(deaths = deathIncrease) %>%
  dplyr::mutate(sos_7d = mav_st(sos)) %>%
  dplyr::mutate(cases_7d = mav_st(cases)) %>%
  dplyr::mutate(deaths_7d = mav_st(deaths)) %>%
  dplyr::select(date, sos, sos_7d, cases, cases_7d, deaths, deaths_7d) %>%
  dplyr::arrange(date)

p_ctp_1 <- ggplot() +
  geom_bar(data = df_ctp_plot, mapping = aes(x = date, y = cases, color = cases, fill = cases), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(data = df_ctp_plot, mapping = aes(x = date, y = cases_7d), size = 1.25) +
  geom_text(data = filter(df_ctp_plot, date == tail(date, n = 1)), aes(label = comma(cases_7d, accuracy = 1), x = date, y = cases_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "EUA: COVID-19", subtitle = "Casos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  scale_y_comma(limits = c(0,325000), breaks = seq(0,300000,50000), name = NULL) +
  scale_x_date(limits = c(filter(df_ctp_plot, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_ctp_2 <- ggplot() +
  geom_bar(data = df_ctp_plot, mapping = aes(x = date, y = sos, color = sos, fill = sos), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(data = df_ctp_plot, mapping = aes(x = date, y = sos_7d), size = 1.25) +
  geom_text(data = filter(df_ctp_plot, date == tail(date, n = 1)), aes(label = comma(sos_7d, accuracy = 1), x = date, y = sos_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = NULL, subtitle = "Hospitalizacoes", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  scale_y_comma(limits = c(0,175000), breaks = seq(0,150000,50000), name = NULL) +
  scale_x_date(limits = c(filter(df_ctp_plot, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_ctp_3 <- ggplot() +
  geom_bar(data = df_ctp_plot, mapping = aes(x = date, y = deaths, color = deaths, fill = deaths), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(data = df_ctp_plot, mapping = aes(x = date, y = deaths_7d), size = 1.25) +
  geom_text(data = filter(df_ctp_plot, date == tail(date, n = 1)), aes(label = comma(deaths_7d, accuracy = 1), x = date, y = deaths_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = NULL, subtitle = "Obitos", caption = "Fonte: The COVID Tracking Project", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  scale_y_comma(limits = c(0,5500), breaks = seq(0,5000,1000), name = NULL) +
  scale_x_date(limits = c(filter(df_ctp_plot, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

## Agregado

p1 <- ggplot() +
  geom_bar(data = df_nyt, mapping = aes(x = date, y = deaths, color = deaths, fill = deaths), position="stack", stat="identity") +
  labs(title = "EUA: COVID-19", subtitle = "Numero de Obitos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(limits = c(-25000,225000), breaks = seq(0,200000,50000), name = "Obitos") +
  scale_y_comma(name = "Obitos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p2 <- ggplot() +
  geom_bar(data = df_nyt, mapping = aes(x = date, y = cases, color = cases, fill = cases), position="stack", stat="identity") +
  labs(title = "EUA: COVID-19", subtitle = "Numero de Casos", caption = "Fonte: New York Times", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(limits = c(-1000000,8000000), breaks = seq(0,6000000,2000000), name = "Casos") +
  scale_y_comma(name = "Casos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p3 <- ggplot() +
  geom_bar(data = df_nyt, mapping = aes(x = date, y = d_deaths, color = deaths, fill = deaths), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(data = df_nyt, mapping = aes(x = date, y = mt_d_deaths, color = mt_d_deaths), size = 1.25) +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(mt_d_deaths, accuracy = 1), x = date, y = mt_d_deaths), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "US: COVID-19", subtitle = "Obitos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(limits = c(filter(df_nyt, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p4 <- ggplot() +
  geom_bar(data = df_nyt, mapping = aes(x = date, y = d_cases, color = cases, fill = cases), position="stack", stat="identity") +
  geom_line(data = df_nyt, mapping = aes(x = date, y = mt_d_cases, color = mt_d_cases), size = 1.25) +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(mt_d_cases, accuracy = 1), x = date, y = mt_d_cases), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "US: COVID-19", subtitle = "Casos", caption = "Fonte: New York Times", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(limits = c(filter(df_nyt, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p5 <- ggplot() +
  geom_line(data = df_nyt, mapping = aes(x = date, y = mt_d_deaths, color = mt_d_deaths), size = 1.25) +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(mt_d_deaths, accuracy = 1), x = date, y = mt_d_deaths), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "US: COVID-19", subtitle = "Variacao Diaria dos Obitos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(limits = c(-500,3500), breaks = seq(0,3000,1000), name = "Obitos") +
  scale_y_comma(name = "Obitos") +
  scale_x_date(limits = c(filter(df_nyt, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p6 <- ggplot() +
  geom_line(data = df_nyt, mapping = aes(x = date, y = mt_d_cases, color = mt_d_cases), size = 1.25) +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(mt_d_cases, accuracy = 1), x = date, y = mt_d_cases), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "US: COVID-19", subtitle = "Variacao Diaria dos Casos", caption = "Fonte: New York Times", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(limits = c(-5000,85000), breaks = seq(0,80000,10000), name = "Casos") +
  scale_y_comma(name = "Casos") +
  scale_x_date(limits = c(filter(df_nyt, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

# grid.arrange(p1,p2,nrow=2,ncol=1)
# grid.arrange(p3,p4,nrow=2,ncol=1)
# grid.arrange(p5,p6,nrow=2,ncol=1)

# dfm_nyt <- melt(df_nyt, id.vars = "date", measure.vars = c("mt_d_deaths", "mt_d_cases"))
coeff <- 10

p100 <- ggplot(data = df_nyt, aes(x = date)) +
  geom_line(mapping = aes(y = mt_d_deaths, color = mt_d_deaths), size = 1.25) +
  geom_line(mapping = aes(y = mt_d_cases/coeff, color = mt_d_cases), size = 1.25) +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(mt_d_deaths, accuracy = 1), x = date, y = mt_d_deaths), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  geom_text(data = filter(df_nyt, date == tail(date, n = 1)), aes(label = comma(mt_d_cases, accuracy = 1), x = date, y = mt_d_cases/coeff), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  annotate("text", x = as.Date("2020-05-04"), y = 1250, label = "Obitos", size = 5, color = "black", fontface = "bold") +
  annotate("text", x = as.Date("2020-07-27"), y = 7500, label = "Casos", size = 5, color = "black", fontface = "bold") +
  labs(title = "EUA: COVID-19", subtitle = "Medias Moveis (7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(0,22500), breaks = seq(0,20000,5000), name = "Obitos", sec.axis = sec_axis(trans = ~.*coeff, breaks = seq(0,200000,50000), name = "Casos")) +
  scale_x_date(limits = c(filter(df_nyt, date == head(date, n = 1))[,1],ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

## Por Estado

p8 <- ggplot() +
  geom_bar(data = df_st_nyt, mapping = aes(x = date, y = deaths, color = state, fill = state), position="stack", stat="identity") +
  labs(title = "EUA: COVID-19", subtitle = "Numero de Obitos", caption = "Fonte: New York Times", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = "Obitos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p9 <- ggplot() +
  geom_bar(data = df_st_nyt, mapping = aes(x = date, y = cases, color = state, fill = state), position="stack", stat="identity") +
  labs(title = "EUA: COVID-19", subtitle = "Numero de Casos", caption = "Fonte: New York Times", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = "Casos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p10 <- ggplot() +
  geom_bar(data = df_st_nyt, mapping = aes(x = date, y = d_deaths, color = state, fill = state), position="stack", stat="identity") +
  # geom_smooth(data = df_st_nyt, mapping = aes(x = date, y = delta)) +
  labs(title = "US: COVID-19", subtitle = "Variacao Diaria dos Obitos", caption = "Fonte: New York Times", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(limits = c(62.5,187.5), breaks = seq(75,175,25), name = "Casos") +
  scale_y_comma(name = "Obitos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  # scale_size_manual(values = c(1.25, 1.25)) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p11 <- ggplot() +
  geom_bar(data = df_st_nyt, mapping = aes(x = date, y = d_cases, color = state, fill = state), position="stack", stat="identity") +
  # geom_smooth(data = df_st_nyt, mapping = aes(x = date, y = delta)) +
  labs(title = "US: COVID-19", subtitle = "Variacao Diaria dos Casos", caption = "Fonte: New York Times", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(limits = c(62.5,187.5), breaks = seq(75,175,25), name = "Casos") +
  scale_y_comma(name = "Casos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  # scale_size_manual(values = c(1.25, 1.25)) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p12 <- ggplot(data = df_rtl_plot, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_deaths, color = region, group = region, fill = region), position="stack", stat="identity") +
  labs(title = "US: COVID-19", subtitle = "Variacao Diaria dos Obitos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(0,3500), breaks = seq(0,3000,1000), name = "Obitos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p13 <- ggplot(data = df_rtl_plot, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_cases, color = region, group = region, fill = region), position="stack", stat="identity") +
  labs(title = "US: COVID-19", subtitle = "Variacao Diaria dos Casos", caption = "Fonte: The COVID Tracking Project", x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(0,110000), breaks = seq(0,100000,20000), name = "Casos") +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# grid.arrange(p12,p13,nrow=2,ncol=1)

### Our World in Data ## Vaccinations

iso_code_vax = c('BRA','CHN','DEU','ESP','FRA','GBR','IND','ISR','ITA','PRT','RUS','USA')

p_vax_1 <- df_owid_vaccinations %>%
  dplyr::filter(iso_code == iso_code_vax) %>%
  dplyr::mutate(d_vax_per_mil = new_vaccinations_smoothed_per_million) %>%
  dplyr::mutate(d_vax = new_vaccinations_smoothed) %>%
  dplyr::select(date,location,d_vax,d_vax_per_mil) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  # geom_bar(mapping = aes(x = date, y = d_vax, color = location, fill = location), position="stack", stat="identity", show.legend = F) +
  geom_area(mapping = aes(x = date, y = d_vax, color = location, fill = location)) +
  geom_line(mapping = aes(x = date, y = d_vax, color = 'black'), size = 1.25) +
  labs(title = "COVID-19", subtitle = "Vacinacoes por Dia", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(date_breaks = '4 months', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

p_vax_2 <- df_owid_vaccinations %>%
  dplyr::filter(iso_code == iso_code_vax) %>%
  dplyr::mutate(d_vax_per_mil = new_vaccinations_smoothed_per_million) %>%
  dplyr::mutate(d_vax = new_vaccinations_smoothed) %>%
  dplyr::select(date,location,d_vax,d_vax_per_mil) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  # geom_bar(mapping = aes(x = date, y = d_vax, color = location, fill = location), position="stack", stat="identity", show.legend = F) +
  geom_area(mapping = aes(x = date, y = d_vax_per_mil, color = location, fill = location)) +
  geom_line(mapping = aes(x = date, y = d_vax_per_mil, color = 'black'), size = 1.25) +
  labs(title = "COVID-19", subtitle = "Vacinacoes por Milhao", caption = 'Fonte: Our World in Data', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(date_breaks = '4 months', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

p_vax_3 <- df_owid_vaccinations %>%
  dplyr::filter(iso_code == iso_code_vax) %>%
  dplyr::mutate(vax_per_hundred = people_vaccinated_per_hundred) %>%
  dplyr::select(date,location,vax_per_hundred) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = vax_per_hundred, color = location, fill = location)) +
  geom_line(mapping = aes(x = date, y = vax_per_hundred, color = 'black'), size = 1.25) +
  labs(title = "COVID-19", subtitle = "Vacinacoes por 100 Habitantes", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(date_breaks = '4 months', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

p_vax_4 <- df_owid_vaccinations %>%
  dplyr::filter(iso_code == iso_code_vax) %>%
  dplyr::mutate(vax_fully_per_hundred = people_fully_vaccinated_per_hundred) %>%
  dplyr::select(date,location,vax_fully_per_hundred) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = vax_fully_per_hundred, color = location, fill = location)) +
  geom_line(mapping = aes(x = date, y = vax_fully_per_hundred, color = 'black'), size = 1.25) +
  labs(title = "COVID-19", subtitle = "Vacinacoes Completas por 100 Habitantes", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(date_breaks = '4 months', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

### Our World in Data ## Brazil

df_owid_bra <- df_owid %>%
  dplyr::group_by(location) %>%
  dplyr::filter(location == 'Brazil') %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(new_cases_7d = mav_st(new_cases)) %>%
  dplyr::mutate(new_deaths_7d = mav_st(new_deaths)) %>%
  dplyr::mutate(new_cases_per_million_7d = mav_st(new_cases_per_million)) %>%
  dplyr::mutate(new_deaths_per_million_7d = mav_st(new_deaths_per_million)) %>%
  dplyr::select(date, new_cases, new_deaths, new_cases_per_million, new_deaths_per_million, new_cases_7d, new_deaths_7d, new_cases_per_million_7d, new_deaths_per_million_7d) %>%
  dplyr::arrange(date)

st_dt <- as.Date(as.numeric(filter(df_owid_bra, date == head(date, n = 1))[,2]))

p_owid_bra_1 <- ggplot(data = df_owid_bra, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_cases, color = new_cases, fill = new_cases), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(y = new_cases_7d), size = 1.25) +
  geom_text(data = filter(df_owid_bra, date == tail(date, n = 1)), aes(label = comma(new_cases_7d, accuracy = 1), x = date, y = new_cases_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "Brasil: COVID-19", subtitle = "Casos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  scale_y_comma(limits = c(0,137500), breaks = seq(0,125000,25000), name = NULL) +
  # scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_x_date(limits = c(as.Date(as.numeric(filter(df_owid_bra, date == head(date, n = 1))[,2])),ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_owid_bra_2 <- ggplot(data = df_owid_bra, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_deaths, color = new_deaths, fill = new_deaths), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(y = new_deaths_7d), size = 1.25) +
  geom_text(data = filter(df_owid_bra, date == tail(date, n = 1)), aes(label = comma(new_deaths_7d, accuracy = 1), x = date, y = new_deaths_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "Brasil: COVID-19", subtitle = "Obitos", caption = 'Fonte: Our World in Data', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(name = NULL) +
  scale_y_comma(limits = c(0,5500), breaks = seq(0,5000,1000), name = NULL) +
  # scale_x_date(date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_x_date(limits = c(as.Date(as.numeric(filter(df_owid_bra, date == head(date, n = 1))[,2])),ed), date_breaks = "5 weeks", date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


### John Hopkins University por pais

ctry <- as.character(c('Brazil','Israel','United.States', 'United.Kingdom'))
n_ctry_col <- 4
n_ctry_row <- 1

p_jhu_ctry_1 <- df_jhu_cases %>%
  pivot_longer(cols = -date, names_to = 'country', values_to= 'values') %>%
  dplyr::arrange(date) %>%
  dplyr::filter(country %in% ctry) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(values_7d = mav_st(values)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = values, color = values, fill = values), position="stack", stat="identity") +
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.125) +
  # geom_text(data = filter(df_jhu_cases, date == tail(date, n = 1) && country == ctry), aes(label = comma(values_7d, accuracy = 1), x = date, y = values_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(date_breaks = "26 weeks", date_labels = "%b %y") +
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
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.125) +
  # geom_text(data = filter(df_jhu_deaths_plot_ctry, date == tail(date, n = 1)), aes(label = comma(values_7d, accuracy = .01), x = date, y = values_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = NULL, subtitle = "Obitos Novos por Dia por 1,000,000 (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(date_breaks = "26 weeks", date_labels = "%b %y") +
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
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.125) +
  # geom_text(data = filter(df_jhu_cases_new_plot_ctry, date == tail(date, n = 1)), aes(label = comma(values_7d, accuracy = 1), x = date, y = values_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "COVID-19", subtitle = "Casos Novos por Dia (MM 7 Dias)", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(date_breaks = "26 weeks", date_labels = "%b %y") +
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
  geom_line(mapping = aes(x = date, y = values_7d), size = 1.125) +
  # geom_text(data = filter(df_jhu_deaths_new_plot_ctry, date == tail(date, n = 1)), aes(label = comma(values_7d, accuracy = 1), x = date, y = values_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "COVID-19", subtitle = "Obitos Novos por Dia (MM 7 Dias)", caption = 'Fonte: Johns Hopkins University', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(name = NULL) +
  scale_x_date(date_breaks = "26 weeks", date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha) +
  facet_wrap(~country, ncol = n_ctry_col/n_ctry_row, nrow = n_ctry_row)

## Grid Plots

grid.arrange(p_vax_1,p_vax_2,nrow=2,ncol=1)
grid.arrange(p_vax_3,p_vax_4,nrow=2,ncol=1)

# p_jhu_ctry_1
# p_jhu_ctry_2
grid.arrange(p_jhu_ctry_1,p_jhu_ctry_2,nrow=2,ncol=1)
grid.arrange(p_jhu_ctry_3,p_jhu_ctry_4,nrow=2,ncol=1)

grid.arrange(p_jhu_1,p_jhu_2,nrow=2,ncol=1)
grid.arrange(p_jhu_3,p_jhu_4,nrow=2,ncol=1)

grid.arrange(p_ctp_1,p_ctp_2,p_ctp_3,nrow=3,ncol=1)
grid.arrange(p_owid_bra_1,p_owid_bra_2,nrow=2,ncol=1)

