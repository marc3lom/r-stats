### initial settings
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/economics/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/economics/')
setwd('/home/marcelom/.local/code/r/economics/')

# locale settings
# windows
# Sys.setlocale('LC_ALL', 'English')
# Sys.setlocale('LC_TIME', 'Portuguese')
# linux
# Sys.setlocale('LC_ALL', 'en_US.UTF-8')
Sys.setlocale('LC_ALL', 'pt_BR.UTF-8')
# Sys.setlocale('LC_TIME', 'pt_BR.UTF-8')

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
library(fredr)

library(dotenv)
# load_dot_env(file = 'd:/usr/marcelom/OneDrive/Code/R/.env')
load_dot_env(file = '/home/marcelom/.env')
# calling .env variable example
fredr_set_key(Sys.getenv('fred_api_key'))
# quandl_api_key(Sys.getenv('quandl_key'))

### ggplot2 settings

theme_set(
  theme_minimal()
)

theme_update(
  legend.position = 'none',
  legend.title = element_blank(),
  legend.text = element_text(face = 'bold'),
  plot.caption = element_text(face = 'bold'),
  axis.text.x = element_text(face = 'bold'),
  axis.text.y = element_text(face = 'bold')
)

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

v_c_alpha <- 1.000 # set value between 0 and 1
v_f_alpha <- v_c_alpha * 0.750

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

# getting and preparing data
# by fred
df <- fredr(
  series_id = 'PAYEMS',
  observation_start = as.Date('1939-01-01'),
  observation_end = as.Date(Sys.Date())
) %>%
  subset(select = c(date,value))

# modifying data

colnames(df) <- c('date','nfp_tot')
df$date <- lubridate::ymd(df$date)

nc <- nrow(df)

myStartDate = as.Date('2017-01-01')
# myStartDate = as.Date('2017-01-31')
myEndDate = as.Date(df$date[nc])

long_lag <- 20
medium_lag <- 10
short_lag <- 5

begin_date_long <- floor_date(Sys.Date() - years(long_lag), unit = 'year') # - days(1)
begin_date_medium <- floor_date(Sys.Date() - years(medium_lag), unit = 'year') # - days(1)
begin_date_short <- floor_date(Sys.Date() - years(short_lag), unit = 'year') # - days(1)
end_date <- ceiling_date(Sys.Date(), unit = 'month')

nfp_cycle_long <- which(grepl(begin_date_long, df$date))
nfp_cycle_medium <- which(grepl(begin_date_medium, df$date))
nfp_cycle_short <- which(grepl(begin_date_short, df$date))

# Modifying Data

df <- df %>% 
  dplyr::mutate(nfp_01m_chg = df$nfp_tot-lag(df$nfp_tot, n = 1)) %>%
  dplyr::mutate(nfp_03m_chg = df$nfp_tot-lag(df$nfp_tot, n = 3)) %>%
  dplyr::mutate(nfp_06m_chg = df$nfp_tot-lag(df$nfp_tot, n = 6)) %>%
  dplyr::mutate(nfp_12m_chg = df$nfp_tot-lag(df$nfp_tot, n = 12)) %>%
  dplyr::mutate(nfp_36m_chg = df$nfp_tot-lag(df$nfp_tot, n = 36)) %>%
  dplyr::mutate(nfp_60m_chg = df$nfp_tot-lag(df$nfp_tot, n = 60))
df <- df %>%
  dplyr::mutate(nfp_03m_mavg = mavg(df$nfp_01m_chg,3)) %>%
  dplyr::mutate(nfp_06m_mavg = mavg(df$nfp_01m_chg,6)) %>%
  dplyr::mutate(nfp_12m_mavg = mavg(df$nfp_01m_chg,12)) %>%
  dplyr::mutate(nfp_cycle_long = df$nfp_tot - df$nfp_tot[nfp_cycle_long]) %>%
  dplyr::mutate(nfp_cycle_medium = df$nfp_tot - df$nfp_tot[nfp_cycle_medium]) %>%
  dplyr::mutate(nfp_cycle_short = df$nfp_tot - df$nfp_tot[nfp_cycle_short])

df_tot <- df %>%
  subset(select = c(date,nfp_tot)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_chg <- df %>%
  subset(select = c(date,nfp_01m_chg,nfp_03m_chg,nfp_06m_chg,nfp_12m_chg,nfp_36m_chg,nfp_60m_chg)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

### GGPlots

df_tot %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  labs(title = "US: Non-Farm Payrolls", subtitle = "Total", caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_x_date(date_labels = "%Y") +
  scale_y_comma(name = "x1000 Pessoas") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

df_tot %>%
  ggplot() +
  geom_path(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  labs(title = "US: Non-Farm Payrolls", subtitle = "Total", caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_x_date(limits = c(as.Date("2010-01-31"), as.Date(Sys.Date())), date_labels = "%Y") +
  scale_y_comma(limits = c(126250,153750), breaks = seq(127500,152500,2500), name = "x1000 Pessoas") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

df_chg %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = values, color = ticker, fill = ticker), size = 1.00, color = 'gray32') +
  labs(title = "US: Non-Farm Payrolls", subtitle = "Variacao por Periodo", caption = "Fonte: Bureau of Labor Statistics", x = NULL, y = NULL) +
  scale_y_comma(limits = c(-22500,22500), breaks = seq(-20000,20000,5000), name = "x 1.000 Pessoas") +
  scale_x_date(limits = c(as.Date("2000-01-01"), as.Date(Sys.Date())), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE) +
  facet_wrap(~ticker)

p0 <- df %>%
  dplyr::select(date, nfp_01m_chg) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>% na.omit() %>%
  ggplot(mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = values, color = values, fill = values), position="stack", stat="identity", color = 'gray64') +
  labs(title = "EUA: Non-Farm Payrolls", subtitle = "Variacao Mensal", caption = NULL, x = NULL, y = NULL) +
  scale_y_comma(limits = c(-2125,2125), breaks = seq(-2000,2000,250), name = "x1000 Pessoas") +
  scale_x_date(expand = c(0,0), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = FALSE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = FALSE)

p1 <- df %>%
  dplyr::select(date, nfp_01m_chg) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = values, color = values, fill = values), position="stack", stat="identity", color = 'gray64') +
  labs(title = "EUA: Non-Farm Payrolls", subtitle = "Variacao Mensal", caption = NULL, x = NULL, y = NULL) +
  scale_y_comma(limits = c(-22500,7500), breaks = seq(-20000,5000,5000), name = "x1000 Pessoas") +
  scale_x_date(date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = FALSE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = FALSE)

p2 <- df %>%
  dplyr::select(date, nfp_01m_chg, nfp_03m_mavg, nfp_06m_mavg, nfp_12m_mavg) %>%
  ggplot(mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = nfp_01m_chg, color = nfp_01m_chg, fill = nfp_01m_chg), position="stack", stat="identity", color = 'gray64') +
  geom_line(mapping = aes(y = nfp_03m_mavg), size = 1.000, alpha = 0.500) +
  geom_line(mapping = aes(y = nfp_06m_mavg), size = 0.875, alpha = 0.750) +
  geom_line(mapping = aes(y = nfp_12m_mavg), size = 0.750, alpha = 1.000) +
  labs(title = "EUA: Non-Farm Payrolls", subtitle = "Variacao Mensal e Medias Moveis (3m, 6m e 12m)", caption = NULL, x = NULL, y = NULL) +
  scale_y_comma(limits = c(-22500,7500), breaks = seq(-20000,5000,5000), name = "x1000 Pessoas") +
  scale_x_date(limits = c(as.Date("2015-12-31"), as.Date(Sys.Date()+35)), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = FALSE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = FALSE)

grid.arrange(p1,p2,ncol=1)

p3 <- df %>%
  dplyr::select(date, nfp_cycle_long) %>%
  ggplot(mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = nfp_cycle_long, color = nfp_cycle_long, fill = nfp_cycle_long), position="stack", stat="identity", color = 'gray64') +
  labs(title = "EUA: Non-Farm Payrolls", subtitle = "Variacao Acumulada (20 Anos)", caption = NULL, x = NULL, y = NULL) +
  scale_y_comma(name = "x1000 Pessoas") +
  scale_x_date(limits = c(begin_date_long,end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = FALSE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = FALSE)

p4 <- df %>%
  dplyr::select(date, nfp_cycle_medium) %>%
  ggplot(mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = nfp_cycle_medium, color = nfp_cycle_medium, fill = nfp_cycle_medium), position="stack", stat="identity", color = 'gray64') +
  labs(title = "EUA: Non-Farm Payrolls", subtitle = "Variacao Acumulada (10 Anos)", caption = NULL, x = NULL, y = NULL) +
  scale_y_comma(name = "x1000 Pessoas") +
  scale_x_date(limits = c(begin_date_medium,end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = FALSE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = FALSE)

p5 <- df %>%
  dplyr::select(date, nfp_cycle_short) %>%
  ggplot(mapping = aes(x = date)) +
  geom_bar(mapping = aes(y = nfp_cycle_short, color = nfp_cycle_short, fill = nfp_cycle_short), position="stack", stat="identity", color = 'gray64') +
  labs(title = "EUA: Non-Farm Payrolls", subtitle = "Variacao Acumulada (5 Anos)", caption = NULL, x = NULL, y = NULL) +
  scale_y_comma(name = "x1000 Pessoas") +
  scale_x_date(limits = c(begin_date_short,end_date), date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = FALSE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = FALSE)

# p3
# p4
# p5

grid.arrange(p3,p4,p5,ncol=1)


