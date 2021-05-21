### initial settings
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/taa/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/taa/')
setwd('/home/marcelom/.local/code/r/finance/')

# locale settings
# windows
# Sys.setlocale('LC_ALL', 'English')
# Sys.setlocale('LC_TIME', 'Portuguese')
# linux
Sys.setlocale('LC_ALL', 'en_US.UTF-8')
# Sys.setlocale('LC_ALL', 'pt_BR.UTF-8')
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
library(timetk)
library(caTools)
library(moments)
library(MASS)
library(metRology)
library(rugarch)
library(PerformanceAnalytics)

library(dotenv)
# load_dot_env(file = 'd:/usr/marcelom/OneDrive/Code/R/.env')
load_dot_env(file = '/home/marcelom/.env')
# calling .env variable example
# quandl_api_key(Sys.getenv('quandl_key'))

### ggplot theme settings

theme_set(theme_minimal())

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
# v_c_alpha <- 0.500
v_f_alpha <- v_c_alpha / 2

### custom functions

# moving average
mavg <- function(x,n=7,s=1){
  stats::filter(
    x,
    rep(
      1/ifelse(s == 1,n,ifelse(n %% 2 == 0,n + 1,n)),
      ifelse(s == 1,n,ifelse(n %% 2 == 0,n + 1,n))
    ),
    sides=s
  )
}

# geometric mean using logs
gm_mean <- function(x){
  exp(
    mean(
      log(x[is.finite(log(x))])
    )
  )
}

### getting and preparing data

# df <- read_xlsx("d:/usr/marcelom/OneDrive/Code/R/taa/depinPerf.xlsx", sheet = "dataRead")
df <- read_xlsx("/home/marcelom/.local/code/r/finance/depinPerf.xlsx", sheet = "dataRead")
df$date <- lubridate::ymd(df$date)

df$real <- rep(100, nrow(df))
df$ref <- rep(100, nrow(df))

for (i in 2:nrow(df)) {
  df$real[i] <- df$real[i-1] *  (1 + df$real_pct[i])
  df$ref[i] <- df$ref[i-1] *  (1 + df$ref_pct[i])
}

df_l <- df %>%
  dplyr::select(date,real,ref) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'index')

# date adjustments

lag_years <- 7
sample_years <- lag_years + 1
offset <- 52/4 # period in weeks to offset plots at beginning and end 

# df_ctaa_dates <- read_xlsx("d:/usr/marcelom/OneDrive/Code/R/taa/depinPerf.xlsx", sheet = "dataRead_dates")
df_ctaa_dates <- read_xlsx("/home/marcelom/.local/code/r/finance/depinPerf.xlsx", sheet = "dataRead_dates")
df_ctaa_dates$date <- lubridate::ymd(df_ctaa_dates$date)

df_dates <- data.frame(year = format(as.Date(character(sample_years)), "%Y"), date = as.Date(character(sample_years)))

for (k in 1:sample_years) {
  df_dates[k,1] <- format(as.Date(Sys.Date() + years(1 - k)), "%Y")
  df_dates[k,2] <- as.Date(floor_date(Sys.Date() + years(1 - k), 'year'))
}

bench_date <- as.Date(lubridate::floor_date(as.Date('2001-07-02'), unit = 'quarter') - days(1))
long_tag_date <- as.Date(lubridate::floor_date(bench_date, unit = 'year'))
long_long_tag_date <- as.Date(lubridate::floor_date(long_tag_date - lubridate::weeks(1), unit = 'years'))
start_date <- as.Date(lubridate::floor_date(as.Date(Sys.Date() - lubridate::years(lag_years)), unit = 'year'))
short_tag_date <- as.Date(lubridate::floor_date(start_date - lubridate::weeks(26), unit = 'quarter'))
sample_date <- as.Date(df_dates[sample_years,2])
lag_date <- as.Date(df_dates[lag_years,2])
ytd_date <- as.Date(df_dates[1,2])
yearago_date <- as.Date(lubridate::floor_date(Sys.Date() - years(1), unit = 'week'))
yearago_plot_start_date <- as.Date(lubridate::floor_date(yearago_date - weeks(2), unit = 'week'))
yearago_plot_end_date <- as.Date(lubridate::ceiling_date(max(df$date) + weeks(2), unit = 'week'))

for (Di in 0:(lag_years)) {
  name_1 <- paste("ctaa", "date", "m", Di, sep = "_")
  assign(name_1, as.Date(as.numeric(df_ctaa_dates[nrow(df_ctaa_dates) - Di,1])))
}

ctaa_date <- ceiling_date(ctaa_date_m_3 + lubridate::weeks(-2), unit = 'week')
end_date <- as.Date(lubridate::ceiling_date(as.Date(Sys.Date()), unit = 'quarter'))
tag_end_date <- as.Date(lubridate::ceiling_date(as.Date(end_date + lubridate::weeks(offset)), unit = 'quarters'))
mid_decade_end <- as.Date('2026-01-01')

# risk parameters

mgri_y <- -0.01
mgri_d <- mgri_y/sqrt(252)
mgri_d_bps <- mgri_d * 10000
cl <- 0.05

# calculating returns

df_drets <- df_l %>%
  dplyr::group_by(portfolio) %>%
  tq_transmute(index,
               periodReturn,
               period = 'daily',
               col_rename = 'returns') %>%
  pivot_wider(names_from = portfolio, values_from = returns) %>%
  dplyr::mutate(alpha = (real - ref) * 10000)

df_lrets <- df_l %>%
  dplyr::group_by(portfolio) %>%
  tq_transmute(index,
               periodReturn,
               period = 'daily',
               type = 'log',
               col_rename = 'log_returns')

df_w_drets <- df_l %>%
  group_by(portfolio) %>%
  tq_transmute(select = index,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns') %>%
  pivot_wider(names_from = portfolio, values_from = returns) %>%
  dplyr::mutate(active = real - ref)

d_ref_avg <- mean(df_drets$ref)
d_ref_sd <- sd(df_drets$ref)
d_ref_var <- quantile(df_drets$ref,cl)
d_ref_es <- mean(df_drets$ref[df_drets$ref<d_ref_var])
d_ref_p_avg <- df_drets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
d_ref_p_sd <- df_drets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()
d_ref_p_var <- df_drets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_var = quantile(ref,cl)) %>% as.numeric()
d_ref_p_es <- df_drets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_es = mean(ref[ref<d_ref_p_var])) %>% as.numeric()
d_ref_yearago_avg <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
d_ref_yearago_sd <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()
d_ref_yearago_var <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_var = quantile(ref,cl)) %>% as.numeric()
d_ref_yearago_es <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_es = mean(ref[ref<d_ref_p_var])) %>% as.numeric()
d_ref_ytd_avg <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
d_ref_ytd_sd <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()
d_ref_ytd_var <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_var = quantile(ref,cl)) %>% as.numeric()
d_ref_ytd_es <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_es = mean(ref[ref<d_ref_ytd_var])) %>% as.numeric()
d_ref_ctaa_avg <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
d_ref_ctaa_sd <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()
d_ref_ctaa_var <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_var = quantile(ref,cl)) %>% as.numeric()
d_ref_ctaa_es <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_es = mean(ref[ref<d_ref_p_var])) %>% as.numeric()

d_alpha_avg <- mean(df_drets$alpha)
d_alpha_sd <- sd(df_drets$alpha)
d_alpha_var <- quantile(df_drets$alpha,cl)
d_alpha_es <- mean(df_drets$alpha[df_drets$alpha<d_alpha_var])
d_alpha_p_avg <- df_drets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
d_alpha_p_sd <- df_drets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()
d_alpha_p_var <- df_drets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_var = quantile(alpha,cl)) %>% as.numeric()
d_alpha_p_es <- df_drets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_es = mean(alpha[alpha<d_alpha_p_var])) %>% as.numeric()
d_alpha_yearago_avg <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
d_alpha_yearago_sd <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()
d_alpha_yearago_var <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_var = quantile(alpha,cl)) %>% as.numeric()
d_alpha_yearago_es <- df_drets %>% filter(date >= yearago_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_es = mean(alpha[alpha<d_alpha_yearago_var])) %>% as.numeric()
d_alpha_ytd_avg <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
d_alpha_ytd_sd <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()
d_alpha_ytd_var <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_var = quantile(alpha,cl)) %>% as.numeric()
d_alpha_ytd_es <- df_drets %>% filter(date >= ytd_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_es = mean(alpha[alpha<d_alpha_ytd_var])) %>% as.numeric()
d_alpha_ctaa_avg <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
d_alpha_ctaa_sd <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()
d_alpha_ctaa_var <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_var = quantile(alpha,cl)) %>% as.numeric()
d_alpha_ctaa_es <- df_drets %>% filter(date >= ctaa_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_es = mean(alpha[alpha<d_alpha_ctaa_var])) %>% as.numeric()

df_wrets <- df_l %>%
  dplyr::group_by(portfolio) %>%
  tq_transmute(index,
               periodReturn,
               period = 'weekly',
               col_rename = 'returns') %>%
  pivot_wider(names_from = portfolio, values_from = returns) %>%
  dplyr::mutate(alpha = (real - ref) * 10000)

w_ref_avg <- mean(df_wrets$ref)
w_ref_sd <- sd(df_wrets$ref)
w_ref_p_avg <- df_wrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
w_ref_p_sd <- df_wrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()

w_alpha_avg <- mean(df_wrets$alpha)
w_alpha_sd <- sd(df_wrets$alpha)
w_alpha_p_avg <- df_wrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
w_alpha_p_sd <- df_wrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()

df_mrets <- df_l %>%
  dplyr::group_by(portfolio) %>%
  tq_transmute(index,
               periodReturn,
               period = 'monthly',
               col_rename = 'returns') %>%
  pivot_wider(names_from = portfolio, values_from = returns) %>%
  dplyr::mutate(alpha =(real - ref)*10000)

m_ref_avg <- mean(df_mrets$ref)
m_ref_sd <- sd(df_mrets$ref)
m_ref_p_avg <- df_mrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
m_ref_p_sd <- df_mrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()

m_alpha_avg <- mean(df_mrets$alpha)
m_alpha_sd <- sd(df_mrets$alpha)
m_alpha_p_avg <- df_mrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
m_alpha_p_sd <- df_mrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()

df_qrets <- df_l %>%
  dplyr::group_by(portfolio) %>%
  tq_transmute(index,
               periodReturn,
               period = 'quarterly',
               col_rename = 'returns') %>%
  pivot_wider(names_from = portfolio, values_from = returns) %>%
  dplyr::mutate(alpha =(real - ref)*10000)

q_ref_avg <- mean(df_qrets$ref)
q_ref_sd <- sd(df_qrets$ref)
q_ref_p_avg <- df_qrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
q_ref_p_sd <- df_qrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()

q_alpha_avg <- mean(df_qrets$alpha)
q_alpha_sd <- sd(df_qrets$alpha)
q_alpha_p_avg <- df_qrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
q_alpha_p_sd <- df_qrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()

df_yrets <- df_l %>%
  dplyr::group_by(portfolio) %>%
  tq_transmute(index,
               periodReturn,
               period = 'yearly',
               col_rename = 'returns') %>%
  pivot_wider(names_from = portfolio, values_from = returns) %>%
  dplyr::mutate(alpha =(real - ref)*10000)

y_ref_avg <- mean(df_yrets$ref)
y_ref_sd <- sd(df_yrets$ref)
y_ref_p_avg <- df_yrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_average = mean(ref)) %>% as.numeric()
y_ref_p_sd <- df_yrets %>% filter(date >= start_date) %>% dplyr::select(ref) %>% dplyr::summarise(partial_stdev = sd(ref)) %>% as.numeric()

y_alpha_avg <- mean(df_yrets$alpha)
y_alpha_sd <- sd(df_yrets$alpha)
y_alpha_p_avg <- df_yrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_average = mean(alpha)) %>% as.numeric()
y_alpha_p_sd <- df_yrets %>% filter(date >= start_date) %>% dplyr::select(alpha) %>% dplyr::summarise(partial_stdev = sd(alpha)) %>% as.numeric()

# Begin Daily Section
# Daily Returns

btow_d_all <- df_drets %>%
  dplyr::arrange(desc(ref))

wtob_d_all <- df_drets %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "all", sep = "_")
  assign(name_1, as.Date(btow_d_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "all", sep = "_")
  assign(name_1, as.Date(wtob_d_all$date)[i])
}

btow_d_partial <- df_drets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(ref))

wtob_d_partial <- df_drets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "partial", sep = "_")
  assign(name_1, as.Date(btow_d_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "partial", sep = "_")
  assign(name_1, as.Date(wtob_d_partial$date)[i])
}

btow_d_ytd <- df_drets %>%
  dplyr::filter(date >= ytd_date) %>%
  dplyr::arrange(desc(ref))

wtob_d_ytd <- df_drets %>%
  dplyr::filter(date >= ytd_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "ytd", sep = "_")
  assign(name_1, as.Date(btow_d_ytd$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "ytd", sep = "_")
  assign(name_1, as.Date(wtob_d_ytd$date)[i])
}

btow_d_ctaa <- df_drets %>%
  dplyr::filter(date >= ctaa_date) %>%
  dplyr::arrange(desc(ref))

wtob_d_ctaa <- df_drets %>%
  dplyr::filter(date >= ctaa_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "ctaa", sep = "_")
  assign(name_1, as.Date(btow_d_ctaa$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "ctaa", sep = "_")
  assign(name_1, as.Date(wtob_d_ctaa$date)[i])
}

btow_d_yadt <- df_drets %>%
  dplyr::filter(date >= yearago_date) %>%
  dplyr::arrange(desc(ref))

wtob_d_yadt <- df_drets %>%
  dplyr::filter(date >= yearago_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "yadt", sep = "_")
  assign(name_1, as.Date(btow_d_yadt$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "yadt", sep = "_")
  assign(name_1, as.Date(wtob_d_yadt$date)[i])
}

# Daily Alpha

btow_d_alpha_all <- df_drets %>%
  dplyr::arrange(desc(alpha))

wtob_d_alpha_all <- df_drets %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "alpha", "all", sep = "_")
  assign(name_1, as.Date(btow_d_alpha_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "alpha", "all", sep = "_")
  assign(name_1, as.Date(wtob_d_alpha_all$date)[i])
}

btow_d_alpha_partial <- df_drets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(alpha))

wtob_d_alpha_partial <- df_drets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(btow_d_alpha_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(wtob_d_alpha_partial$date)[i])
}


btow_d_alpha_ytd <- df_drets %>%
  dplyr::filter(date >= ytd_date) %>%
  dplyr::arrange(desc(alpha))

wtob_d_alpha_ytd <- df_drets %>%
  dplyr::filter(date >= ytd_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "alpha", "ytd", sep = "_")
  assign(name_1, as.Date(btow_d_alpha_ytd$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "alpha", "ytd", sep = "_")
  assign(name_1, as.Date(wtob_d_alpha_ytd$date)[i])
}

btow_d_alpha_ctaa <- df_drets %>%
  dplyr::filter(date >= ctaa_date) %>%
  dplyr::arrange(desc(alpha))

wtob_d_alpha_ctaa <- df_drets %>%
  dplyr::filter(date >= ctaa_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "alpha", "ctaa", sep = "_")
  assign(name_1, as.Date(btow_d_alpha_ctaa$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "alpha", "ctaa", sep = "_")
  assign(name_1, as.Date(wtob_d_alpha_ctaa$date)[i])
}

btow_d_alpha_yadt <- df_drets %>%
  dplyr::filter(date >= yearago_date) %>%
  dplyr::arrange(desc(alpha))

wtob_d_alpha_yadt <- df_drets %>%
  dplyr::filter(date >= yearago_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "d", "alpha", "yadt", sep = "_")
  assign(name_1, as.Date(btow_d_alpha_yadt$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "d", "alpha", "yadt", sep = "_")
  assign(name_1, as.Date(wtob_d_alpha_yadt$date)[i])
}

all_acum <- df_lrets %>%
  pivot_wider(names_from = portfolio, values_from = log_returns) %>%
  dplyr::mutate(alpha = (exp(cumsum(real)) - exp(cumsum(ref))) * 10000) %>%
  dplyr::select(date,alpha) %>%
  dplyr::arrange(date)

partial_acum <- df_lrets %>%
  dplyr::filter(date >= start_date) %>%
  pivot_wider(names_from = portfolio, values_from = log_returns) %>%
  dplyr::mutate(alpha = (exp(cumsum(real)) - exp(cumsum(ref))) * 10000) %>%
  dplyr::select(date,alpha) %>%
  dplyr::arrange(date)

ytd_acum <- df_lrets %>%
  dplyr::filter(date >= ytd_date) %>%
  pivot_wider(names_from = portfolio, values_from = log_returns) %>%
  dplyr::mutate(alpha = (exp(cumsum(real)) - exp(cumsum(ref))) * 10000) %>%
  dplyr::select(date,alpha) %>%
  dplyr::arrange(date)

yearago_acum <- df_lrets %>%
  dplyr::filter(date >= yearago_date) %>%
  pivot_wider(names_from = portfolio, values_from = log_returns) %>%
  dplyr::mutate(alpha = (exp(cumsum(real)) - exp(cumsum(ref))) * 10000) %>%
  dplyr::select(date,alpha) %>%
  dplyr::arrange(date)

ctaa_acum <- df_lrets %>%
  dplyr::filter(date >= ctaa_date) %>%
  pivot_wider(names_from = portfolio, values_from = log_returns) %>%
  dplyr::mutate(alpha = (exp(cumsum(real)) - exp(cumsum(ref))) * 10000) %>%
  dplyr::select(date,alpha) %>%
  dplyr::arrange(date)

var_offset <- 7/16

# Daily Plots

p1 <- ggplot() +
  geom_bar(data = df_drets, mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_hline(yintercept = d_ref_avg + d_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_ref_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_ref_avg - d_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = d_ref_avg, label = percent(d_ref_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = d_ref_var, label = percent(d_ref_var, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_var>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = d_ref_es, label = percent(d_ref_es, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Di?rios", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-.0175,.0175), breaks = seq(-0.015,0.015,.005), accuracy = 0.1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p2 <- df_drets %>%
  dplyr::filter(date >= start_date) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  # geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  # geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  # geom_vline(xintercept = ctaa_date_m_2, size = 0.75, color = "orangered", alpha = 0.50) +
  # geom_vline(xintercept = ctaa_date_m_3, size = 0.75, color = "orangered", alpha = 0.50) +
  # geom_vline(xintercept = ctaa_date_m_4, size = 0.75, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_ref_p_avg + d_ref_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_ref_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_ref_p_avg - d_ref_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = ceiling_date(start_date - weeks(8), unit = 'week'), y = d_ref_p_avg, label = percent(d_ref_p_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_p_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = ceiling_date(start_date - weeks(8), unit = 'week'), y = d_ref_p_var, label = percent(d_ref_p_var, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_p_var>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_p_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = ceiling_date(start_date - weeks(8), unit = 'week'), y = d_ref_p_es, label = percent(d_ref_p_es, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_p_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Di?rios", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.009,0.009), breaks = seq(-0.008,0.008,0.002), accuracy = 0.1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_ytd_1 <- df_drets %>%
  dplyr::filter(date >= ytd_date) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_ytd), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -0.006, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = d_ref_ytd_avg + d_ref_ytd_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_ref_ytd_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_ref_ytd_avg - d_ref_ytd_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = as.Date('2020-12-30'), y = d_ref_ytd_avg, label = percent(d_ref_ytd_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_ytd_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_ytd_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2020-12-30'), y = d_ref_ytd_var, label = percent(d_ref_ytd_var, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_ytd_var>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_ytd_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2020-12-30'), y = d_ref_ytd_es, label = percent(d_ref_ytd_es, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_ytd_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Di?rios", subtitle = paste('Ano Corrente (Comit?-TAA: ', ctaa_date_m_0, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.0025,0.0025), breaks = seq(-0.002,0.002,0.001), accuracy = 0.1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_yearago_1 <- df_drets %>%
  dplyr::filter(date >= yearago_date) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_yadt), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_0, y = -.0075, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_1, y = -.0075, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  # geom_vline(xintercept = ctaa_date_m_2, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date_m_2, y = -.0075, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = d_ref_yearago_avg + d_ref_yearago_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_ref_yearago_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_ref_yearago_avg - d_ref_yearago_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = as.Date(yearago_plot_start_date), y = d_ref_yearago_avg, label = percent(d_ref_yearago_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_yearago_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_yearago_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date(yearago_plot_start_date), y = d_ref_yearago_var, label = percent(d_ref_yearago_var, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_yearago_var>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_yearago_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date(yearago_plot_start_date), y = d_ref_yearago_es, label = percent(d_ref_yearago_es, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_yearago_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Di?rios", subtitle = paste('?ltimas 52 Semanas (Comit?-TAA: ', ctaa_date, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.00875,0.00875), breaks = seq(-.0075,0.0075,0.0025), accuracy = 0.01) +
  # scale_x_date(date_breaks = '5 weeks', date_labels = "%d %b") +
  # scale_x_date(limits = c(yearago_plot_start_date, yearago_plot_end_date), date_breaks = '5 weeks', date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_ctaa_1 <- df_drets %>%
  dplyr::filter(date >= ctaa_date) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_ctaa), aes(label = percent(ref, accuracy = 0.001), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -0.006, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = d_ref_ctaa_avg + d_ref_ctaa_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_ref_ctaa_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_ref_ctaa_avg - d_ref_ctaa_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = as.Date(ctaa_date - lubridate::days(5)), y = d_ref_ctaa_avg, label = percent(d_ref_ctaa_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_ctaa_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_ctaa_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date(ctaa_date - lubridate::days(5)), y = d_ref_ctaa_var, label = percent(d_ref_ctaa_var, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_ctaa_var>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_ref_ctaa_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date(ctaa_date - lubridate::days(5)), y = d_ref_ctaa_es, label = percent(d_ref_ctaa_es, accuracy = 0.001), size = 3.5, color = ifelse(d_ref_ctaa_es>=0,'springgreen4','red3'), fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_0, y = -0.006, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_1, y = -0.006, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_2, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_2, y = -0.006, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_3, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_3, y = -0.006, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Di?rios", subtitle = paste('Quatro ?ltimas Reuni?es do Comit? TAA (', ctaa_date_m_3, '), (', ctaa_date_m_2, '), (', ctaa_date_m_1, ') e (', ctaa_date_m_0, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  # labs(title = "Carteira de Refer?ncia: Retornos Di?rios", subtitle = paste('Desde as Quatro ?ltimas Reuni?es do Comit? TAA (', ctaa_date_m_0, ') e (', ctaa_date_m_1, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_percent(accuracy = 0.01) +
  scale_y_percent(limits = c(-0.007,0.007), breaks = seq(-0.006,0.006,0.002), accuracy = 0.1) +
  scale_x_date(date_breaks = '13 weeks', date_labels = "%b %y") +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p3 <- ggplot() +
  geom_bar(data = df_drets, mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_hline(yintercept = d_alpha_avg + d_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_avg - d_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = d_alpha_avg, label = comma(d_alpha_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = d_alpha_var, label = comma(d_alpha_var, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_var>=0,'springgreen4','red3'), fontface = "bold") +
  annotate("text", x = ceiling_date(long_tag_date + weeks(3*52), unit = 'week'), y = d_alpha_var, label = percent(d_alpha_var/mgri_d_bps, accuracy = 0.001), size = 3.5, color = ifelse((d_alpha_var/mgri_d_bps)>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = d_alpha_es, label = comma(d_alpha_es, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Di?rios (bps)", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-17.5,17.5), breaks = seq(-15,15,5), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p4 <- df_drets %>%
  dplyr::filter(date >= start_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_hline(yintercept = d_alpha_p_avg + d_alpha_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_p_avg - d_alpha_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = ceiling_date(start_date - weeks(8), unit = 'week'), y = d_alpha_p_avg, label = comma(d_alpha_p_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_p_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = ceiling_date(start_date - weeks(8), unit = 'week'), y = d_alpha_p_var, label = comma(d_alpha_p_var, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_p_var>=0,'springgreen4','red3'), fontface = "bold") +
  annotate("text", x = ceiling_date(start_date + weeks(16), unit = 'week'), y = d_alpha_p_var, label = percent(d_alpha_p_var/mgri_d_bps, accuracy = 0.001), size = 3.5, color = ifelse((d_alpha_p_var/mgri_d_bps)>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_p_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = ceiling_date(start_date - weeks(8), unit = 'week'), y = d_alpha_p_es, label = comma(d_alpha_p_es, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_p_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Di?rios (bps)", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_y_comma(limits = c(-7,7), breaks = seq(-6,6,2), accuracy = 1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_ytd_2 <- df_drets %>%
  dplyr::filter(date >= ytd_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_alpha_ytd), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  # geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  # geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date_m_0, y = -1.5, label = 'Comit? TAA', size = 3.5, color = "orangered", fontface = "bold") +
  # annotate("text", x = ctaa_date_m_1, y = -1.5, label = 'C-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = d_alpha_ytd_avg + d_alpha_ytd_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_ytd_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_ytd_avg - d_alpha_ytd_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = as.Date('2020-12-30'), y = d_alpha_ytd_avg, label = comma(d_alpha_ytd_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_ytd_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_ytd_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2020-12-30'), y = d_alpha_ytd_var, label = comma(d_alpha_ytd_var, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_ytd_var>=0,'springgreen4','red3'), fontface = "bold") +
  annotate("text", x = ceiling_date(as.Date('2020-12-30'), unit = 'week'), y = d_alpha_ytd_var, label = percent(d_alpha_ytd_var/mgri_d_bps, accuracy = 0.001), size = 3.5, color = ifelse((d_alpha_ytd_var/mgri_d_bps)>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_ytd_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2020-12-30'), y = d_alpha_ytd_es, label = comma(d_alpha_ytd_es, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_ytd_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Di?rios (bps)", subtitle = paste('Ano Corrente (Comit?-TAA: ', ctaa_date_m_0, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma() +
  scale_y_comma(limits = c(-0.875,0.875), breaks = seq(-0.75,0.75,0.25), accuracy = .01) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_yearago_2 <- df_drets %>%
  dplyr::filter(date >= yearago_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_b_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_b_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_b_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_b_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_1_w_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_2_w_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_3_w_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_4_w_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_text(data = filter(df_drets, date == dt_5_w_d_alpha_yadt), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_0, y = -1.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_1, y = -1.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  # geom_vline(xintercept = ctaa_date_m_2, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date_m_2, y = -1.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = d_alpha_yearago_avg + d_alpha_yearago_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_yearago_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_yearago_avg - d_alpha_yearago_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = as.Date(yearago_plot_start_date), y = d_alpha_yearago_avg, label = comma(d_alpha_yearago_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_yearago_avg>=0,'springgreen4','red3'), fontface = "bold") +
  # geom_hline(yintercept = d_alpha_yearago_var , size = 0.75, color = "orangered4", alpha = 0.50) +
  # annotate("text", x = as.Date(yearago_plot_start_date), y = d_alpha_yearago_var * (1 - var_offset), label = comma(d_alpha_yearago_var, accuracy = 0.01), size = 3.5, color = ifelse(d_alpha_yearago_var>=0,'springgreen4','red3'), fontface = "bold") +
  # annotate("text", x = as.Date(yearago_plot_start_date), y = d_alpha_yearago_var * (1 + var_offset), label = percent(d_alpha_yearago_var/mgri_d_bps, accuracy = 0.01), size = 3.5, color = ifelse((d_alpha_yearago_var/mgri_d_bps)>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_yearago_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = yearago_plot_start_date, y = d_alpha_yearago_var, label = comma(d_alpha_yearago_var, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_yearago_var>=0,'springgreen4','red3'), fontface = "bold") +
  annotate("text", x = ceiling_date(yearago_plot_start_date + weeks(2), unit = 'week'), y = d_alpha_yearago_var, label = percent(d_alpha_yearago_var/mgri_d_bps, accuracy = 0.001), size = 3.5, color = ifelse((d_alpha_yearago_var/mgri_d_bps)>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_yearago_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = yearago_plot_start_date, y = d_alpha_yearago_es, label = comma(d_alpha_yearago_es, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_yearago_es>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Di?rios (bps)", subtitle = paste('?ltimas 52 Semanas (Comit?-TAA: ', ctaa_date, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-1.75,1.75), breaks = seq(-1.5,1.5,0.5), accuracy = .1) +
  # scale_x_date(date_breaks = '5 weeks', date_labels = "%d %b") +
  # scale_x_date(limits = c(yearago_plot_start_date, yearago_plot_end_date), date_breaks = '5 weeks', date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p_ctaa_2 <- df_drets %>%
  dplyr::filter(date >= ctaa_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = date), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_drets, date == dt_1_b_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_2_b_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_3_b_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_4_b_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_5_b_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_1_w_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_2_w_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_3_w_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_4_w_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_drets, date == dt_5_w_d_alpha_ctaa), aes(label = comma(alpha, accuracy = 0.001), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = d_alpha_ctaa_avg + d_alpha_ctaa_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_ctaa_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = d_alpha_ctaa_avg - d_alpha_ctaa_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = as.Date(ctaa_date - lubridate::days(5)), y = d_alpha_ctaa_avg, label = comma(d_alpha_ctaa_avg, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_ctaa_avg>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_ctaa_var , size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date(ctaa_date - lubridate::days(5)), y = d_alpha_ctaa_var, label = comma(d_alpha_ctaa_var, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_ctaa_var>=0,'springgreen4','red3'), fontface = "bold") +
  annotate("text", x = ctaa_date_m_2, y = d_alpha_ctaa_var, label = percent(d_alpha_ctaa_var/mgri_d_bps, accuracy = 0.001), size = 3.5, color = ifelse((d_alpha_ctaa_var/mgri_d_bps)>=0,'springgreen4','red3'), fontface = "bold") +
  geom_hline(yintercept = d_alpha_ctaa_es , size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date(ctaa_date - lubridate::days(5)), y = d_alpha_ctaa_es, label = comma(d_alpha_ctaa_es, accuracy = 0.001), size = 3.5, color = ifelse(d_alpha_ctaa_es>=0,'springgreen4','red3'), fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_0, y = -1.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_1, y = -1.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_2, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_2, y = -1.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_3, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_3, y = -1.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Di?rios (bps)", subtitle = paste('Quatro ?ltimas Reuni?es do Comit? TAA (', ctaa_date_m_3, '), (', ctaa_date_m_2, '), (', ctaa_date_m_1, ') e (', ctaa_date_m_0, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_comma(accuracy = 0.01) +
  scale_y_comma(limits = c(-1.75,1.75), breaks = seq(-1.5,1.5,0.5), accuracy = .1) +
  scale_x_date(date_breaks = '13 weeks', date_labels = "%b %y") +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_breaks = '13 weeks', date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

plot_all_acum <- all_acum %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = alpha), size = 1.25) +
  geom_text(data = filter(all_acum, date == tail(date, n = 1)), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), hjust = -.125, position = position_dodge(.9), size = 3.5, color = 'black', fontface = "bold") +
  labs(title = "Gest?o Ativa: Retorno Acumulado (bps)", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-25,325), breaks = seq(0,300,50), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, ceiling_date(Sys.Date() + weeks(4), unit = 'month'))) +
  scale_color_viridis_d(option = "E", alpha = 0.75) +
  scale_fill_viridis_d(option = "E", alpha = 0.50)

plot_partial_acum <- partial_acum %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = alpha), size = 1.25) +
  geom_text(data = filter(partial_acum, date == tail(date, n = 1)), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), hjust = -.125, position = position_dodge(.9), size = 3.5, color = 'black', fontface = "bold") +
  labs(title = "Gest?o Ativa: Retorno Acumulado (bps)", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-7.5,32.5), breaks = seq(-5,30,5), accuracy = 1) +
  # scale_x_date(limits = c(as.Date(start_date), ceiling_date(Sys.Date() + weeks(1), unit = 'week'))) +
  scale_color_viridis_d(option = "E", alpha = 0.75) +
  scale_fill_viridis_d(option = "E", alpha = 0.50)

plot_ytd_acum <- ytd_acum %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = alpha), size = 1.25) +
  geom_text(data = filter(ytd_acum, date == tail(date, n = 1)), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), hjust = -.125, position = position_dodge(.9), size = 3.5, color = 'black', fontface = "bold") +
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -2, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  labs(title = "Gest?o Ativa: Retorno Acumulado (bps)", subtitle = paste('Ano Corrente (Comit?-TAA: ', ctaa_date_m_0, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.1)) +
  # scale_y_comma(limits = c(-3,13), breaks = seq(-2,12,2), accuracy = 1) +
  # scale_x_date(limits = c(as.Date(ytd_date), as.Date(Sys.Date()+1)), date_breaks = '2 weeks', date_labels = "%d %b") +
  scale_x_date(date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

plot_yearago_acum <- yearago_acum %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = alpha), size = 1.25) +
  geom_text(data = filter(yearago_acum, date == tail(date, n = 1)), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), hjust = -.125, position = position_dodge(.9), size = 3.5, color = 'black', fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_0, y = -1.25, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_1, y = -1.25, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  # geom_vline(xintercept = ctaa_date_m_2, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date_m_2, y = -2, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  labs(title = "Gest?o Ativa: Retorno Acumulado (bps)", subtitle = paste('?ltimas 52 Semanas (Comit?-TAA: ', ctaa_date, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-3,17), breaks = seq(-2,16,2), accuracy = 1) +
  # scale_x_date(limits = c(yearago_plot_start_date, yearago_plot_end_date), date_breaks = '5 weeks', date_labels = "%d %b") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

plot_ctaa_acum <- ctaa_acum %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = alpha), size = 1.25) +
  geom_text(data = filter(ctaa_acum, date == tail(date, n = 1)), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), hjust = -.125, position = position_dodge(.9), size = 3.5, color = 'black', fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_0, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_0, y = -2.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_1, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_1, y = -2.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_2, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_2, y = -2.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_vline(xintercept = ctaa_date_m_3, size = 0.75, color = "orangered", alpha = 0.50) +
  annotate("text", x = ctaa_date_m_3, y = -2.5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  labs(title = "Gest?o Ativa: Retorno Acumulado (bps)", subtitle = paste('Quatro ?ltimas Reuni?es do Comit? TAA (', ctaa_date_m_3, '), (', ctaa_date_m_2, '), (', ctaa_date_m_1, ') e (', ctaa_date_m_0, ')', sep = ''), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(date_breaks = '13 weeks', date_labels = "%b %y") +
  scale_y_comma(accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

df_seag_full <- df_w_drets %>%
  dplyr::select(date,active) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  group_by(portfolio) %>%
  mutate(year = year(date)) %>%
  mutate(day = as.numeric(format(date, "%j"))) %>%
  group_by(year) %>%
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cumulative_returns = (cr - 1) * 10000)

df_seag_full_year_end <- df_seag_full %>%
  dplyr::filter(day == max(day))

df_seag_full_ranked <- df_seag_full_year_end[order(df_seag_full_year_end$cumulative_returns),]

p_seag_full <- ggplot(data = df_seag_full) +
  geom_path(mapping = aes(x = day, y = cumulative_returns, color = year, group = year), size = 1.125) +
  geom_hline(yintercept = 0, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_text(data = filter(df_seag_full_year_end, year == df_seag_full_ranked[1,4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  # geom_text(data = filter(df_seag_full_year_end, year == df_seag_full_ranked[2,4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  # geom_text(data = filter(df_seag_full_year_end, year == df_seag_full_ranked[3,4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  geom_text(data = filter(df_seag_full, year == (year(Sys.Date()) - 0) & day == tail(day, n = 1)), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 4.0, fontface = "bold") +
  # geom_text(data = filter(df_seag_full_year_end, year == df_seag_full_ranked[nrow(df_seag_full_ranked)-2,4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  # geom_text(data = filter(df_seag_full_year_end, year == df_seag_full_ranked[nrow(df_seag_full_ranked)-1,4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  geom_text(data = filter(df_seag_full_year_end, year == df_seag_full_ranked[nrow(df_seag_full_ranked),4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  labs(title = "Gest?o Ativa: Retorno Acumulado (bps)", subtitle = 'Desde 2001', caption = NULL, x = 'Dias', y = 'Retorno Acumulado (em bps)') +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_comma(limits = c(0,425), breaks = seq(0,400,100), accuracy = 1) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  # scale_y_comma(limits = c(-45,65), breaks = seq(-40,60,20), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = 0.75) + 
  scale_fill_viridis(option = v_color, alpha = 0.50)

df_seag_partial <- df_w_drets %>%
  dplyr::filter(date >= lag_date) %>%
  dplyr::select(date,active) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  group_by(portfolio) %>%
  mutate(year = year(date)) %>%
  mutate(day = as.numeric(format(date, "%j"))) %>%
  group_by(year) %>%
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cumulative_returns = (cr - 1) * 10000)

df_seag_partial_year_end <- df_seag_partial %>%
  dplyr::filter(day == max(day))

df_seag_partial_ranked <- df_seag_partial_year_end[order(df_seag_partial_year_end$cumulative_returns),]

p_seag_partial <- ggplot() +
  geom_path(data = df_seag_partial, mapping = aes(x = day, y = cumulative_returns, color = year, group = year), size = 1.125) +
  geom_text(data = filter(df_seag_partial_year_end, year == df_seag_partial_ranked[1,4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  geom_text(data = filter(df_seag_partial, year == (year(Sys.Date()) - 0) & day == tail(day, n = 1)), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 4.0, fontface = "bold") +
  geom_text(data = filter(df_seag_partial_year_end, year == df_seag_partial_ranked[nrow(df_seag_partial_ranked),4]), aes(label = paste(comma(year, accuracy = 1, big.mark = ""), '|', comma(cumulative_returns, accuracy = .01, big.mark = ""),  sep = " "), x = 370, y = cumulative_returns), hjust = -.25, position = position_dodge(.9), size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 0, size = 0.25, color = "orangered", alpha = 0.50) +
  labs(title = "Gest?o Ativa: Retorno Acumulado (bps)", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = 'Dias', y = 'Retorno Acumulado (em bps)') +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_comma(limits = c(0,425), breaks = seq(0,400,100), accuracy = 1) +
  # scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_y_comma(limits = c(-7.5,27.5), breaks = seq(-5,25,5), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = 0.75) +
  scale_fill_viridis(option = v_color, alpha = 0.50)

df_drets_acum <- df_drets %>%
  dplyr::mutate(real_acum = cumprod(1 + real) - 1) %>%
  dplyr::mutate(ref_acum = cumprod(1 + ref) - 1) %>%
  dplyr::mutate(alpha_acum = real_acum - ref_acum) %>%
  dplyr::select(date,real_acum,ref_acum,alpha_acum)

p_d_acum_1 <- df_drets_acum %>%
  dplyr::select(date,real_acum,ref_acum) %>%
  pivot_longer(!date, names_to = 'portfolio', values_to = 'returns') %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = returns, color = portfolio, group = portfolio), size = 1) +
  # geom_hline(yintercept = 0, size = 0.875, color = "orangered", alpha = 0.75) +
  geom_text(data = filter(df_drets_acum, date == tail(date, n = 1)), aes(label = paste('real', '|', percent(real_acum, accuracy = 0.01), sep = ' '), x = date, y = real_acum), hjust = -.25, position = position_dodge(.9), color = "darkblue", size = 3.5, fontface = "bold") +
  geom_text(data = filter(df_drets_acum, date == tail(date, n = 1)), aes(label = paste('ref', '|', percent(ref_acum, accuracy = 0.01), sep = ' '), x = date, y = ref_acum), hjust = -.25, position = position_dodge(.9), color = "darkred", size = 3.5, fontface = "bold") +
  labs(title = "Reservas: Retorno Acumulado", subtitle = 'Desde Julho/2001', caption = NULL, x = NULL, y = 'Retorno Acumulado') +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_x_date(limits = c(long_tag_date, floor_date(as.Date(ceiling_date(tag_end_date + 365, 'year')), 'year')), date_labels = "%Y") +
  scale_y_percent(limits = c(-0.1,.9), breaks = seq(0,.8,.2), accuracy = 1) +
  scale_color_viridis(option = v_color, discrete = TRUE, alpha = 0.75) +
  scale_fill_viridis(option = v_color, discrete = TRUE, alpha = 0.50)

p_d_acum_2 <- df_drets_acum %>%
  dplyr::select(date,alpha_acum) %>%
  pivot_longer(!date, names_to = 'portfolio', values_to = 'returns') %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = returns, color = portfolio, group = portfolio), size = 1) +
  # geom_hline(yintercept = 0, size = 0.875, color = "orangered", alpha = 0.75) +
  geom_text(data = filter(df_drets_acum, date == tail(date, n = 1)), aes(label = percent(alpha_acum, accuracy = 0.01), x = date, y = alpha_acum), hjust = -.25, position = position_dodge(.9), color = "darkblue", size = 3.5, fontface = "bold") +
  labs(title = "Gest?o Ativa: Resultado Acumulado", subtitle = 'Desde Julho/2001', caption = NULL, x = NULL, y = 'Retorno Acumulado') +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_x_date(limits = c(long_tag_date, floor_date(as.Date(ceiling_date(tag_end_date + 365, 'year')), 'year')), date_labels = "%Y") +
  scale_y_percent(limits = c(-0.0025,.0325), breaks = seq(0,.03,.005), accuracy = .1) +
  scale_color_viridis(option = v_color, discrete = TRUE, alpha = 0.75) +
  scale_fill_viridis(option = v_color, discrete = TRUE, alpha = 0.50)

df_drets_acum_short <- df_drets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::mutate(real_acum = cumprod(1 + real) - 1) %>%
  dplyr::mutate(ref_acum = cumprod(1 + ref) - 1) %>%
  dplyr::mutate(alpha_acum = real_acum - ref_acum) %>%
  dplyr::select(date,real_acum,ref_acum,alpha_acum)

p_d_acum_3 <- df_drets_acum_short %>%
  dplyr::select(date,real_acum,ref_acum) %>%
  pivot_longer(!date, names_to = 'portfolio', values_to = 'returns') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = returns, color = portfolio, group = portfolio), size = 1) +
  # geom_hline(yintercept = 0, size = 0.875, color = "orangered", alpha = 0.75) +
  geom_text(data = filter(df_drets_acum_short, date == tail(date, n = 1)), aes(label = paste('real', '|', percent(real_acum, accuracy = 0.01), sep = ' '), x = head(df_drets_acum_short$date, n = 1), y = real_acum), hjust = -.25, position = position_dodge(.9), color = "darkblue", size = 3.5, fontface = "bold") +
  geom_text(data = filter(df_drets_acum_short, date == tail(date, n = 1)), aes(label = paste('ref', '|', percent(ref_acum, accuracy = 0.01), sep = ' '), x = head(df_drets_acum_short$date, n = 1), y = ref_acum), hjust = -.25, position = position_dodge(.9), color = "darkred", size = 3.5, fontface = "bold") +
  labs(title = "Reservas: Retorno Acumulado", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = 'Retorno Acumulado') +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_x_date(limits = c(start_date, floor_date(as.Date(ceiling_date(tag_end_date, 'year')), 'year')), date_labels = "%Y") +
  scale_y_percent(accuracy = 1) +
  # scale_y_percent(limits = c(-0.0125,.1625), breaks = seq(0,.15,.025), accuracy = .1) +
  scale_color_viridis(option = v_color, discrete = TRUE, alpha = 0.75) +
  scale_fill_viridis(option = v_color, discrete = TRUE, alpha = 0.50)

p_d_acum_4 <- df_drets_acum_short %>%
  dplyr::select(date,alpha_acum) %>%
  pivot_longer(!date, names_to = 'portfolio', values_to = 'returns') %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = returns, color = portfolio, group = portfolio), size = 1) +
  # geom_hline(yintercept = 0, size = 0.875, color = "orangered", alpha = 0.75) +
  geom_text(data = filter(df_drets_acum_short, date == tail(date, n = 1)), aes(label = percent(alpha_acum, accuracy = 0.01), x = date, y = alpha_acum), hjust = -.25, position = position_dodge(.9), color = "darkblue", size = 3.5, fontface = "bold") +
  labs(title = "Gest?o Ativa: Resultado Acumulado", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = 'Retorno Acumulado') +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_x_date(limits = c(start_date, floor_date(as.Date(ceiling_date(tag_end_date, 'year')), 'year')), date_labels = "%Y") +
  scale_y_percent(limits = c(-0.00075,.00325), breaks = seq(-0.0005,.003,.0005), accuracy = .01) +
  scale_color_viridis(option = v_color, discrete = TRUE, alpha = 0.75) +
  scale_fill_viridis(option = v_color, discrete = TRUE, alpha = 0.50)


# End Daily Section

# Begin Weekly Section
# Weekly Returns

df_wrets[nrow(df_wrets), 1] <- ceiling_date(as.Date(Sys.Date()), 'week') - days(2)

btow_w_all <- df_wrets %>%
  dplyr::arrange(desc(ref))

wtob_w_all <- df_wrets %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "w", "all", sep = "_")
  assign(name_1, as.Date(btow_w_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "w", "all", sep = "_")
  assign(name_1, as.Date(wtob_w_all$date)[i])
}

btow_w_partial <- df_wrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(ref))

wtob_w_partial <- df_wrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "w", "partial", sep = "_")
  assign(name_1, as.Date(btow_w_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "w", "partial", sep = "_")
  assign(name_1, as.Date(wtob_w_partial$date)[i])
}

# Weekly Alpha

btow_w_alpha_all <- df_wrets %>%
  dplyr::arrange(desc(alpha))

wtob_w_alpha_all <- df_wrets %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "w", "alpha", "all", sep = "_")
  assign(name_1, as.Date(btow_w_alpha_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "w", "alpha", "all", sep = "_")
  assign(name_1, as.Date(wtob_w_alpha_all$date)[i])
}

btow_w_alpha_partial <- df_wrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(alpha))

wtob_w_alpha_partial <- df_wrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "w", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(btow_w_alpha_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "w", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(wtob_w_alpha_partial$date)[i])
}

# Weekly Plots

p5 <- ggplot() +
  geom_bar(data = df_wrets, mapping = aes(x = date, y = ref, color = ref, fill = ref), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_wrets, date == dt_1_b_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_b_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_b_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_b_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_b_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_1_w_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_w_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_w_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_w_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_w_w_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = w_ref_avg + w_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = w_ref_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = w_ref_avg - w_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = w_ref_avg, label = percent(w_ref_avg, accuracy = 0.01), size = 3.5, color = ifelse(w_ref_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Semanais", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.025,0.025), breaks = seq(-0.02,0.02,0.01), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p6 <- df_wrets %>%
  dplyr::filter(date >= start_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_wrets, date == dt_1_b_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_b_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_b_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_b_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_b_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_1_w_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_w_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_w_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_w_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_w_w_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -0.015, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = w_ref_p_avg + w_ref_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = w_ref_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = w_ref_p_avg - w_ref_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = short_tag_date, y = w_ref_p_avg, label = percent(w_ref_p_avg, accuracy = 0.01), size = 3.5, color = ifelse(w_ref_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Semanais", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.0175,0.0175), breaks = seq(-0.015,0.015,0.005), accuracy = 0.1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p7 <- ggplot() +
  geom_bar(data = df_wrets, mapping = aes(x = date, y = alpha, color = alpha, fill = alpha), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_wrets, date == dt_1_b_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_b_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_b_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_b_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_b_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_1_w_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_w_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_w_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_w_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_w_w_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = w_alpha_avg + w_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = w_alpha_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = w_alpha_avg - w_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = w_alpha_avg, label = comma(w_alpha_avg, accuracy = 0.01), size = 3.5, color = ifelse(w_alpha_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Semanais (bps)", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-22.5,22.5), breaks = seq(-20,20,5), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p8 <- df_wrets %>%
  dplyr::filter(date >= start_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha), position = "stack" , stat = "identity") +
  geom_text(data = filter(df_wrets, date == dt_1_b_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_b_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_b_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_b_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_b_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_1_w_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_2_w_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_3_w_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_4_w_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_wrets, date == dt_5_w_w_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -5, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = w_alpha_p_avg + w_alpha_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = w_alpha_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = w_alpha_p_avg - w_alpha_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = short_tag_date, y = w_alpha_p_avg, label = comma(w_alpha_p_avg, accuracy = 0.01), size = 3.5, color = ifelse(w_alpha_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Semanais (bps)", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-5.5,5.5), breaks = seq(-5,5,1), accuracy = 1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

# End Weekly Section

# Begin Monthly Section
# Monthly Returns

df_mrets[nrow(df_mrets), 1] <- ceiling_date(as.Date(Sys.Date()), 'month') - days(1)

btow_m_all <- df_mrets %>%
  dplyr::arrange(desc(ref))

wtob_m_all <- df_mrets %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "m", "all", sep = "_")
  assign(name_1, as.Date(btow_m_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "m", "all", sep = "_")
  assign(name_1, as.Date(wtob_m_all$date)[i])
}

btow_m_partial <- df_mrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(ref))

wtob_m_partial <- df_mrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "m", "partial", sep = "_")
  assign(name_1, as.Date(btow_m_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "m", "partial", sep = "_")
  assign(name_1, as.Date(wtob_m_partial$date)[i])
}

# Monthly Alpha

btow_m_alpha_all <- df_mrets %>%
  dplyr::arrange(desc(alpha))

wtob_m_alpha_all <- df_mrets %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "m", "alpha", "all", sep = "_")
  assign(name_1, as.Date(btow_m_alpha_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "m", "alpha", "all", sep = "_")
  assign(name_1, as.Date(wtob_m_alpha_all$date)[i])
}

btow_m_alpha_partial <- df_mrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(alpha))

wtob_m_alpha_partial <- df_mrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "m", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(btow_m_alpha_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "m", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(wtob_m_alpha_partial$date)[i])
}

# Monthly Plots

p9 <- ggplot() +
  geom_bar(data = df_mrets, mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity", width = 100/4) +
  geom_text(data = filter(df_mrets, date == dt_1_b_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_b_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_b_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_b_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_b_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_1_w_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_w_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_w_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_w_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_w_m_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = m_ref_avg + m_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = m_ref_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = m_ref_avg - m_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = m_ref_avg, label = percent(m_ref_avg, accuracy = 0.01), size = 3.5, color = ifelse(m_ref_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Mensais", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_percent(limits = c(-0.035,0.035), breaks = seq(-0.03,.03,.01), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p10 <- df_mrets %>%
  dplyr::filter(date >= start_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity", width = 100/4) +
  geom_text(data = filter(df_mrets, date == dt_1_b_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_b_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_b_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_b_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_b_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_1_w_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_w_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_w_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_w_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_w_m_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -0.015, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = m_ref_p_avg + m_ref_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = m_ref_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = m_ref_p_avg - m_ref_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = short_tag_date, y = m_ref_p_avg, label = percent(m_ref_p_avg, accuracy = 0.01), size = 3.5, color = ifelse(m_ref_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Mensais", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_percent(limits = c(-0.0175,0.0175), breaks = seq(-0.015,0.015,0.005), accuracy = 0.1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p11 <- ggplot() +
  geom_bar(data = df_mrets, mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = date), position = "stack" , stat = "identity", width = 100/4) +
  geom_text(data = filter(df_mrets, date == dt_1_b_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_b_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_b_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_b_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_b_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_1_w_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_w_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_w_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_w_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_w_m_alpha_all), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = m_alpha_avg + m_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = m_alpha_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = m_alpha_avg - m_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = m_alpha_avg, label = comma(m_alpha_avg, accuracy = 0.01), size = 3.5, color = ifelse(m_alpha_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Mensais (bps)", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_y_comma(limits = c(-25,25), breaks = seq(-20,20,10), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p12 <- df_mrets %>%
  dplyr::filter(date >= start_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = date), position = "stack" , stat = "identity", width = 100/4) +
  geom_text(data = filter(df_mrets, date == dt_1_b_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_b_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_b_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_b_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_b_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_1_w_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_2_w_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_3_w_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_4_w_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_mrets, date == dt_5_w_m_alpha_partial), aes(label = comma(alpha, accuracy = 0.01), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -8, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = m_alpha_p_avg + m_alpha_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = m_alpha_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = m_alpha_p_avg - m_alpha_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = short_tag_date, y = m_alpha_p_avg, label = comma(m_alpha_p_avg, accuracy = 0.01), size = 3.5, color = ifelse(m_alpha_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Mensais (bps)", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_y_comma(limits = c(-9,9), breaks = seq(-8,8,2), accuracy = 1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

# End Monthly Section

# Begin Quarterly Section
# Quarterly Returns

df_qrets[nrow(df_qrets),1] <- ceiling_date(as.Date(Sys.Date()), 'quarter') - days(1)

btow_q_all <- df_qrets %>%
  dplyr::arrange(desc(ref))

wtob_q_all <- df_qrets %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "q", "all", sep = "_")
  assign(name_1, as.Date(btow_q_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "q", "all", sep = "_")
  assign(name_1, as.Date(wtob_q_all$date)[i])
}

btow_q_partial <- df_qrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(ref))

wtob_q_partial <- df_qrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "q", "partial", sep = "_")
  assign(name_1, as.Date(btow_q_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "q", "partial", sep = "_")
  assign(name_1, as.Date(wtob_q_partial$date)[i])
}

# Quarterly Alpha

btow_q_alpha_all <- df_qrets %>%
  dplyr::arrange(desc(alpha))

wtob_q_alpha_all <- df_qrets %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "q", "alpha", "all", sep = "_")
  assign(name_1, as.Date(btow_q_alpha_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "q", "alpha", "all", sep = "_")
  assign(name_1, as.Date(wtob_q_alpha_all$date)[i])
}

btow_q_alpha_partial <- df_qrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(alpha))

wtob_q_alpha_partial <- df_qrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "q", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(btow_q_alpha_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "q", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(wtob_q_alpha_partial$date)[i])
}

# Quarterly Plots

p13 <- ggplot() +
  geom_bar(data = df_qrets, mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity", width = 100/2) +
  geom_text(data = filter(df_qrets, date == dt_1_b_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_b_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_b_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_b_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_b_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_1_w_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_w_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_w_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_w_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_w_q_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = q_ref_avg + q_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = q_ref_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = q_ref_avg - q_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = q_ref_avg, label = percent(q_ref_avg, accuracy = 0.01), size = 3.5, color = ifelse(q_ref_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Trimestrais", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_percent(limits = c(-0.07,0.07), breaks = seq(-0.06,0.06,0.02), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p14 <- df_qrets %>%
  dplyr::filter(date >= start_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref, group = date), position = "stack" , stat = "identity", width = 100/2) +
  geom_text(data = filter(df_qrets, date == dt_1_b_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_b_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_b_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_b_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_b_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_1_w_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_w_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_w_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_w_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_w_q_partial), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -0.02, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = q_ref_p_avg + q_ref_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = q_ref_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = q_ref_p_avg - q_ref_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = short_tag_date, y = q_ref_p_avg, label = percent(q_ref_p_avg, accuracy = 0.01), size = 3.5, color = ifelse(q_ref_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Trimestrais", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_percent(limits = c(-0.035,0.035), breaks = seq(-.03,.03,.01), accuracy = 1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p15 <- ggplot() +
  geom_bar(data = df_qrets, mapping = aes(x = date, y = alpha, color = alpha, fill = alpha), position = "stack" , stat = "identity", width = 100/2) +
  geom_text(data = filter(df_qrets, date == dt_1_b_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_b_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_b_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_b_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_b_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_1_w_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_w_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_w_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_w_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_w_q_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = q_alpha_avg + q_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = q_alpha_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = q_alpha_avg - q_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_tag_date, y = q_alpha_avg, label = comma(q_alpha_avg, accuracy = 0.01), size = 3.5, color = ifelse(q_alpha_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Trimestrais (bps)", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-45,45), breaks = seq(-40,40,10), accuracy = 1) +
  # scale_x_date(limits = c(long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p16 <- df_qrets %>%
  dplyr::filter(date >= start_date) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha), position = "stack" , stat = "identity", width = 100/2) +
  geom_text(data = filter(df_qrets, date == dt_1_b_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_b_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_b_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_b_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_b_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_1_w_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_2_w_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_3_w_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_4_w_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_qrets, date == dt_5_w_q_alpha_partial), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  # geom_vline(xintercept = ctaa_date, size = 0.75, color = "orangered", alpha = 0.50) +
  # annotate("text", x = ctaa_date, y = -10, label = 'Comit?-TAA', size = 3.5, color = "orangered", fontface = "bold") +
  geom_hline(yintercept = q_alpha_p_avg + q_alpha_p_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = q_alpha_p_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = q_alpha_p_avg - q_alpha_p_sd , size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = short_tag_date, y = q_alpha_p_avg, label = comma(q_alpha_p_avg, accuracy = 0.01), size = 3.5, color = ifelse(q_alpha_p_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Trimestrais (bps)", subtitle = paste('?ltimos', lag_years, 'Anos', sep = ' '), caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-11,11), breaks = seq(-10,10,2), accuracy = 1) +
  # scale_x_date(limits = c(short_tag_date, tag_end_date), date_labels = "%b %y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

# End Quarterly Section

# Begin Yearly Section
# Yearly Returns

df_yrets[nrow(df_yrets),1] <- ceiling_date(as.Date(Sys.Date()), 'year') - days(1)

btow_y_all <- df_yrets %>%
  dplyr::arrange(desc(ref))

wtob_y_all <- df_yrets %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "y", "all", sep = "_")
  assign(name_1, as.Date(btow_y_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "y", "all", sep = "_")
  assign(name_1, as.Date(wtob_y_all$date)[i])
}

btow_y_partial <- df_yrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(ref))

wtob_y_partial <- df_yrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(ref)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "y", "partial", sep = "_")
  assign(name_1, as.Date(btow_y_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "y", "partial", sep = "_")
  assign(name_1, as.Date(wtob_y_partial$date)[i])
}

# Yearly Alpha

btow_y_alpha_all <- df_yrets %>%
  dplyr::arrange(desc(alpha))

wtob_y_alpha_all <- df_yrets %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "y", "alpha", "all", sep = "_")
  assign(name_1, as.Date(btow_y_alpha_all$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "y", "alpha", "all", sep = "_")
  assign(name_1, as.Date(wtob_y_alpha_all$date)[i])
}

btow_y_alpha_partial <- df_yrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(desc(alpha))

wtob_y_alpha_partial <- df_yrets %>%
  dplyr::filter(date >= start_date) %>%
  dplyr::arrange(alpha)

for (i in 1:5) {
  name_1 <- paste("dt", i, "b", "y", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(btow_y_alpha_partial$date)[i])
}

for (i in 1:5) {
  name_1 <- paste("dt", i, "w", "y", "alpha", "partial", sep = "_")
  assign(name_1, as.Date(wtob_y_alpha_partial$date)[i])
}

# Yearly Plots

p17 <- ggplot() +
  geom_bar(data = df_yrets, mapping = aes(x = date, y = ref, color = ref, fill = ref), position = "stack" , stat = "identity", width = 256) +
  geom_text(data = filter(df_yrets, date == dt_1_b_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_2_b_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_3_b_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_4_b_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_5_b_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_1_w_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_2_w_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_3_w_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_4_w_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_5_w_y_all), aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = y_ref_avg + y_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = y_ref_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = y_ref_avg - y_ref_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_long_tag_date, y = y_ref_avg, label = percent(y_ref_avg, accuracy = 0.01), size = 3.5, color = ifelse(y_ref_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Anuais", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.125,0.125), breaks = seq(-0.1,0.1,0.05), accuracy = 1) +
  # scale_x_date(limits = c(long_long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p19 <- ggplot() +
  geom_bar(data = df_yrets, mapping = aes(x = date, y = alpha, color = alpha, fill = alpha), position = "stack" , stat = "identity", width = 256) +
  geom_text(data = filter(df_yrets, date == dt_1_b_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = 5, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_2_b_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_3_b_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_4_b_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_5_b_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = - 1.5, size = 3.5, color = "darkgreen", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_1_w_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_2_w_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_3_w_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_4_w_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_text(data = filter(df_yrets, date == dt_5_w_y_alpha_all), aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), position = position_dodge(.9), vjust = + 1.5, size = 3.5, color = "darkred", fontface = "bold") + # , vjust = +.125, hjust = NULL
  geom_hline(yintercept = y_alpha_avg + y_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  geom_hline(yintercept = y_alpha_avg, size = 0.25, color = "orangered", alpha = 0.50) +
  geom_hline(yintercept = y_alpha_avg - y_alpha_sd, size = 0.25, color = "black", alpha = 0.50) +
  annotate("text", x = long_long_tag_date, y = y_alpha_avg, label = comma(y_alpha_avg, accuracy = 0.01), size = 3.5, color = ifelse(y_alpha_avg>=0,'springgreen4','red3'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Anuais (bps)", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-70,70), breaks = seq(-60,60,20), accuracy = 1) +
  # scale_x_date(limits = c(long_long_tag_date, tag_end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p20 <- df_yrets %>%
  dplyr::select(date,ref) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = ref, color = ref, fill = ref, group = ref), position = "stack" , stat = "identity", width = 256) +
  geom_text(aes(label = percent(ref, accuracy = 0.01), x = date, y = ref), vjust = ifelse(df_yrets$ref >= 0, -1, 1) * 1.375, size = 4.0, color = ifelse(df_yrets$ref >= 0, "darkgreen", "darkred"), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Anuais", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_percent(limits = c(-.075,.125), breaks = seq(-0.05,.10,.05), accuracy = 1) +
  # scale_x_date(limits = c(long_long_tag_date, mid_decade_end), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p21 <- df_yrets %>%
  dplyr::select(date,alpha) %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = alpha, color = alpha, fill = alpha, group = alpha), position = "stack" , stat = "identity", width = 256) +
  geom_text(aes(label = comma(alpha, accuracy = 0.1), x = date, y = alpha), vjust = ifelse(df_yrets$alpha >= 0, -1, 1) * 1.375, size = 4.0, color = ifelse(df_yrets$alpha >= 0, "darkgreen", "darkred"), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Anuais (bps)", subtitle = "Desde Julho/2001", caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  # scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_y_comma(limits = c(-30,70), breaks = seq(-20,60,20), accuracy = 1) +
  # scale_x_date(limits = c(long_long_tag_date, mid_decade_end), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


# End Yearly Section

# Start Chart Grids

# acf(df_drets$ref)
# acf(abs(df_drets$ref))
# 
# acf(df_drets$alpha)
# acf(abs(df_drets$alpha))

# grid.arrange(p1,p3,ncol=1,nrow=2)
# grid.arrange(p5,p7,ncol=1,nrow=2)
# grid.arrange(p9,p11,ncol=1,nrow=2)
# grid.arrange(p13,p15,ncol=1,nrow=2)
# grid.arrange(p17,p19,ncol=1,nrow=2)

grid.arrange(p2,p4,ncol=1,nrow=2)
plot_partial_acum
grid.arrange(p6,p8,ncol=1,nrow=2)
grid.arrange(p10,p12,ncol=1,nrow=2)
grid.arrange(p14,p16,ncol=1,nrow=2)

grid.arrange(p_yearago_1,p_yearago_2,ncol=1,nrow=2)
plot_yearago_acum

grid.arrange(p_ytd_1,p_ytd_2,ncol=1,nrow=2)
plot_ytd_acum

grid.arrange(p_ctaa_1,p_ctaa_2,ncol=1,nrow=2)
plot_ctaa_acum

grid.arrange(p_seag_full,p_seag_partial,ncol=1,nrow=2)
# p_seag_full
# p_seag_partial

# grid.arrange(p_d_acum_1,p_d_acum_2,ncol=1,nrow=2)
# grid.arrange(p_d_acum_3,p_d_acum_4,ncol=1,nrow=2)
grid.arrange(p20,p21,ncol=1,nrow=2)
plot_all_acum

# End Chart Grids
