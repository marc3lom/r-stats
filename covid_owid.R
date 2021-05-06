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

df_owid <- as.data.frame(read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv', stringsAsFactors = F))
df_owid$date <- lubridate::ymd(df_owid$date)

start_date <- as.Date(head(df_owid$date, n = 1))
vax_start_date <- as.Date('2020-12-01')
end_date <- lubridate::ceiling_date(Sys.Date() + lubridate::weeks(2))

iso_code_vax = c('BRA','CHN','DEU','CAN','GBR','IND','ISR','ITA','CHL','USA','PRT','ESP')

df_owid_subset <- df_owid %>%
  dplyr::arrange(location,date) %>% dplyr::filter(iso_code == iso_code_vax) %>% dplyr::filter(date >= start_date) %>% 
  dplyr::select(date,location,new_cases_smoothed,new_deaths_smoothed,new_cases_smoothed_per_million,new_deaths_smoothed_per_million,icu_patients,icu_patients_per_million,hosp_patients,hosp_patients_per_million,total_vaccinations,people_vaccinated,people_fully_vaccinated,people_vaccinated_per_hundred,people_fully_vaccinated_per_hundred,new_vaccinations_smoothed_per_million,new_vaccinations_smoothed,population)

vax_start_date <- as.Date('2020-12-01')
end_date <- as.Date(tail(df_owid_subset$date, n = 1))

### GGPlots

# Our World in Data # Vaccinations

p_vax_1 <- df_owid_subset %>%
  dplyr::arrange(date) %>% dplyr::filter(date >= vax_start_date) %>%
  dplyr::mutate(d_vax_per_mil = new_vaccinations_smoothed_per_million) %>%
  dplyr::mutate(d_vax = new_vaccinations_smoothed) %>%
  dplyr::select(date,location,d_vax,d_vax_per_mil) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = d_vax, color = location, fill = location), size = 1.25, color = 'black') +
  labs(title = "COVID-19", subtitle = "Vacinacoes por Dia", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

p_vax_2 <- df_owid_subset %>%
  dplyr::mutate(d_vax_per_mil = new_vaccinations_smoothed_per_million) %>%
  dplyr::mutate(d_vax = new_vaccinations_smoothed) %>%
  dplyr::select(date,location,d_vax,d_vax_per_mil) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = d_vax_per_mil, color = location, fill = location), size = 1.25, color = 'black') +
  labs(title = "COVID-19", subtitle = "Vacinacoes por Milhao", caption = 'Fonte: Our World in Data', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(limits = c(vax_start_date,end_date), date_breaks = '6 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

p_vax_3 <- df_owid_subset %>%
  dplyr::mutate(vax_per_hundred = people_vaccinated_per_hundred) %>%
  dplyr::select(date,location,vax_per_hundred) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = vax_per_hundred, color = location, fill = location), size = 1.25, color = 'black') +
  labs(title = "COVID-19", subtitle = "Vacinacoes por 100 Habitantes", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(limits = c(vax_start_date,end_date), date_breaks = '6 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

p_vax_4 <- df_owid_subset %>%
  dplyr::mutate(vax_fully_per_hundred = people_fully_vaccinated_per_hundred) %>%
  dplyr::select(date,location,vax_fully_per_hundred) %>%
  dplyr::group_by(location) %>%
  dplyr::arrange(date) %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = vax_fully_per_hundred, color = location, fill = location), size = 1.25, color = 'black') +
  labs(title = "COVID-19", subtitle = "Vacinacoes Completas por 100 Habitantes", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(accuracy = 1, name = NULL) +
  scale_x_date(limits = c(vax_start_date,end_date), date_breaks = '6 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~location)

# Our World in Data # Brazil

df_owid_bra <- df_owid %>%
  dplyr::group_by(location) %>%
  dplyr::filter(location == 'Brazil') %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(new_cases_7d = mav_st(new_cases)) %>%
  dplyr::mutate(new_deaths_7d = mav_st(new_deaths)) %>%
  dplyr::mutate(new_cases_per_million_7d = mav_st(new_cases_per_million)) %>%
  dplyr::mutate(new_deaths_per_million_7d = mav_st(new_deaths_per_million)) %>%
  dplyr::select(date, location, new_cases, new_deaths, new_cases_per_million, new_deaths_per_million, new_cases_7d, new_deaths_7d, new_cases_per_million_7d, new_deaths_per_million_7d) %>%
  dplyr::arrange(date)

p_owid_bra_1 <- ggplot(data = df_owid_bra, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_cases, color = new_cases, fill = new_cases), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(y = new_cases_7d), size = 1.25) +
  geom_text(data = filter(df_owid_bra, date == tail(date, n = 1)), aes(label = comma(new_cases_7d, accuracy = 1), x = date, y = new_cases_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "Brasil: COVID-19", subtitle = "Casos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(0,137500), breaks = seq(0,125000,25000), name = NULL) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_owid_bra_2 <- ggplot(data = df_owid_bra, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_deaths, color = new_deaths, fill = new_deaths), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(y = new_deaths_7d), size = 1.25) +
  geom_text(data = filter(df_owid_bra, date == tail(date, n = 1)), aes(label = comma(new_deaths_7d, accuracy = 1), x = date, y = new_deaths_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "Brasil: COVID-19", subtitle = "Obitos", caption = 'Fonte: Our World in Data', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(0,5500), breaks = seq(0,5000,1000), name = NULL) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

# Our World in Data # USA

df_owid_usa <- df_owid %>%
  dplyr::group_by(location) %>%
  dplyr::filter(location == 'United States') %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(new_cases_7d = mav_st(new_cases)) %>%
  dplyr::mutate(new_deaths_7d = mav_st(new_deaths)) %>%
  dplyr::mutate(new_cases_per_million_7d = mav_st(new_cases_per_million)) %>%
  dplyr::mutate(new_deaths_per_million_7d = mav_st(new_deaths_per_million)) %>%
  dplyr::select(date, location, new_cases, new_deaths, new_cases_per_million, new_deaths_per_million, new_cases_7d, new_deaths_7d, new_cases_per_million_7d, new_deaths_per_million_7d) %>%
  dplyr::arrange(date)

p_owid_usa_1 <- ggplot(data = df_owid_usa, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_cases, color = new_cases, fill = new_cases), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(y = new_cases_7d), size = 1.25) +
  geom_text(data = filter(df_owid_usa, date == tail(date, n = 1)), aes(label = comma(new_cases_7d, accuracy = 1), x = date, y = new_cases_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "EUA: COVID-19", subtitle = "Casos", caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(0,275000), breaks = seq(0,250000,50000), name = NULL) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_owid_usa_2 <- ggplot(data = df_owid_usa, mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = new_deaths, color = new_deaths, fill = new_deaths), position="stack", stat="identity", show.legend = TRUE) +
  geom_line(mapping = aes(y = new_deaths_7d), size = 1.25) +
  geom_text(data = filter(df_owid_usa, date == tail(date, n = 1)), aes(label = comma(new_deaths_7d, accuracy = 1), x = date, y = new_deaths_7d), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "EUA: COVID-19", subtitle = "Obitos", caption = 'Fonte: Our World in Data', x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(0,5500), breaks = seq(0,5000,1000), name = NULL) +
  scale_x_date(limits = c(start_date,end_date), date_breaks = '16 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

### displaying plot grids

grid.arrange(p_vax_1,p_vax_2,nrow=2,ncol=1)
grid.arrange(p_vax_3,p_vax_4,nrow=2,ncol=1)

grid.arrange(p_owid_usa_1,p_owid_usa_2,nrow=2,ncol=1)
grid.arrange(p_owid_bra_1,p_owid_bra_2,nrow=2,ncol=1)

