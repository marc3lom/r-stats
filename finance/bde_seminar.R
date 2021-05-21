### initial settings
rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/saa/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/saa/')
setwd('/home/marcelom/.local/code/r/finance/')

# locale settings
# windows
# Sys.setlocale('LC_ALL', 'English')
# Sys.setlocale('LC_TIME', 'Portuguese')
# linux
Sys.setlocale('LC_ALL', 'en_US.UTF-8')
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

library(dotenv)
# load_dot_env(file = 'd:/usr/marcelom/OneDrive/Code/R/.env')
load_dot_env(file = '/home/marcelom/.env')
# calling .env variable example
# quandl_api_key(Sys.getenv('quandl_key'))

### ggplot2 settings

# theme_set(theme_bw())
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
v_color <- as.character('D') # viridis
# v_color <- as.character('E') # cividis
# v_color <- as.character('F') # rocket
# v_color <- as.character('G') # mako
# v_color <- as.character('H') # turbo

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

### getting and preparing data

# uncomment lines below for windows systems
# x <- read_xlsx('d:/usr/marcelom/OneDrive/Code/R/saa/bde_202105.xlsx', sheet = 'dataRead');x <- ncol(x) - 1
# df_all <- read_xlsx('d:/usr/marcelom/OneDrive/Code/R/saa/bde_202105.xlsx', sheet = 'dataRead', col_types = c('guess', rep('numeric', times = x)))

# uncomment lines below for linux systems
x <- read_xlsx('/home/marcelom/.local/code/r/finance/bde_202105.xlsx', sheet = 'dataRead');x <- ncol(x) - 1
df_all <- read_xlsx('/home/marcelom/.local/code/r/finance/bde_202105.xlsx', sheet = 'dataRead', col_types = c('guess', rep('numeric', times = x)))

df_all$date <- lubridate::ymd(df_all$date)

start_date <- as.Date(head(df_all$date, n = 1))
end_date <- as.Date(Sys.Date())

df_bra_fx <- df_all %>% subset(select = c(date,brl)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')
df_bra_rsrv <- df_all %>% subset(select = c(date,bzidintl)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_us_ylds <- df_all %>% subset(select = c(date,usgg2yr,usgg5yr,usgg10yr))
colnames(df_us_ylds) <- c('date','us_02y','us_05y','us_10y')
df_us_ylds <- df_us_ylds %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_de_ylds <- df_all %>% subset(select = c(date,gdbr2,gdbr5,gdbr10))
colnames(df_de_ylds) <- c('date','de_02y','de_05y','de_10y')
df_de_ylds <- df_de_ylds %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_jp_ylds <- df_all %>% subset(select = c(date,gjgb2,gjgb5,gjgb10))
colnames(df_jp_ylds) <- c('date','jp_02y','jp_05y','jp_10y')
df_jp_ylds <- df_jp_ylds %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_uk_ylds <- df_all %>% subset(select = c(date,gukg2,gukg5,gukg10))
colnames(df_uk_ylds) <- c('date','uk_02y','uk_05y','uk_10y')
df_uk_ylds <- df_uk_ylds %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values')

df_fi_indx <- df_all %>%
  subset(select = c(date,luattruu,i00824us,luactruu,i32561us,lumstruu,lbustruu)) %>% na.omit()
colnames(df_fi_indx) <- c('date','us_trsy','g6_trsy','us_corp','cn_trsy','us_mbs','us_agg')
df_fi_indx <- df_fi_indx %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(date) %>%
  dplyr::mutate(lrets = c(diff(log(values)),NA)) %>% na.omit() %>%
  dplyr::mutate(lrets_rsum = rollsum(x = lrets, 252*5, align = 'right', fill = NA)) %>% na.omit() %>%
  dplyr::mutate(arets_rsum = exp(lrets_rsum) - 1) %>%
  dplyr::mutate(lrets_acum = cumsum(lrets)) %>%
  dplyr::mutate(arets_acum = exp(lrets_acum) - 1)

df_eq_indx <- df_all %>%
  subset(select = c(date,mxus,mxea)) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(date) %>%
  dplyr::mutate(lrets = c(diff(log(values)),NA)) %>% na.omit() %>%
  dplyr::mutate(lrets_rsum = rollsum(x = lrets, 252*5, align = 'right', fill = NA)) %>% na.omit() %>%
  dplyr::mutate(arets_rsum = exp(lrets_rsum) - 1) %>%
  dplyr::mutate(lrets_acum = cumsum(lrets)) %>%
  dplyr::mutate(arets_acum = exp(lrets_acum) - 1)

df_fi_etfs_0 <- df_all %>%
  subset(select = c(date,lumstruu,brtptruu,mbb,vmbs)) %>% na.omit()
colnames(df_fi_etfs_0) <- c('date','us_mbs','us_remix','ishares_mbb','vanguard_vmbs')
df_fi_etfs_0 <- df_fi_etfs_0 %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(date) %>%
  dplyr::mutate(lrets = c(diff(log(values)),NA)) %>% na.omit() %>%
  # dplyr::mutate(lrets_rsum = rollsum(x = lrets, 252*5, align = 'right', fill = NA)) %>% na.omit() %>%
  # dplyr::mutate(arets_rsum = exp(lrets_rsum) - 1) %>%
  dplyr::mutate(lrets_acum = cumsum(lrets)) %>%
  dplyr::mutate(arets = exp(lrets) - 1) %>%
  dplyr::mutate(arets_acum = exp(lrets_acum) - 1)

df_fi_etfs_1 <- df_all %>%
  subset(select = c(date,luactruu,iboxig,lqd)) %>% na.omit()
colnames(df_fi_etfs_1) <- c('date','us_corp','us_iboxx','ishares_iboxx')
df_fi_etfs_1 <- df_fi_etfs_1 %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(date) %>%
  dplyr::mutate(lrets = c(diff(log(values)),NA)) %>% na.omit() %>%
  # dplyr::mutate(lrets_rsum = rollsum(x = lrets, 252*5, align = 'right', fill = NA)) %>% na.omit() %>%
  # dplyr::mutate(arets_rsum = exp(lrets_rsum) - 1) %>%
  dplyr::mutate(lrets_acum = cumsum(lrets)) %>%
  dplyr::mutate(arets = exp(lrets) - 1) %>%
  dplyr::mutate(arets_acum = exp(lrets_acum) - 1)

df_fi_etfs_2 <- df_all %>%
  subset(select = c(date,lbustruu,agg,bnd)) %>% na.omit()
colnames(df_fi_etfs_2) <- c('date','us_agg','ishares_agg','vanguard_bnd')
df_fi_etfs_2 <- df_fi_etfs_2 %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(date) %>%
  dplyr::mutate(lrets = c(diff(log(values)),NA)) %>% na.omit() %>%
  # dplyr::mutate(lrets_rsum = rollsum(x = lrets, 252*5, align = 'right', fill = NA)) %>% na.omit() %>%
  # dplyr::mutate(arets_rsum = exp(lrets_rsum) - 1) %>%
  dplyr::mutate(lrets_acum = cumsum(lrets)) %>%
  dplyr::mutate(arets = exp(lrets) - 1) %>%
  dplyr::mutate(arets_acum = exp(lrets_acum) - 1)

df_eq_etfs <- df_all %>%
  subset(select = c(date,spxt,spy))
colnames(df_eq_etfs) <- c('date','us_spx','spdr_spy')
df_eq_etfs <- df_eq_etfs %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  dplyr::group_by(ticker) %>% dplyr::arrange(date) %>%
  dplyr::mutate(lrets = c(diff(log(values)),NA)) %>% na.omit() %>%
  dplyr::mutate(lrets_rsum = rollsum(x = lrets, 252*5, align = 'right', fill = NA)) %>% na.omit() %>%
  dplyr::mutate(arets_rsum = exp(lrets_rsum) - 1) %>%
  dplyr::mutate(lrets_acum = cumsum(lrets)) %>%
  dplyr::mutate(arets_acum = exp(lrets_acum) - 1)

df_stats <- data.frame(
  year = seq.Date(from = as.Date('2002-01-01'), to = as.Date('2020-01-01'), by = 'years'),
  return = c(.082,.0961,.0502,-.0358,.0603,.0935,.0933,.0083,.0182,.036,.0184,-.0146,-.0064,-.016,.007,.0227,.0117,.0433,.0557),
  duration = c(1.15,1.03,1.01,1.16,1.58,2.38,2.88,2.21,1.68,2.65,2.78,2.52,1.99,2.07,1.93,1.81,2.07,2.71,2.63),
  pct_usd = c(.723,.583,.546,.732,.883,.9,.891,.819,.818,.7961,.774,.777,.797,.8295,.8346,.8225,.8993,.8677,.8603),
  pct_govt_fi = c(.5619,.456,.525,.686,.718,.844,.785,.898,.802,.8351,.8987,.9117,.8999,.8847,.887,.8764,.9318,.9303,.8818)
) %>%
  dplyr::mutate(pct_other_ccys = 1 - pct_usd) %>%
  dplyr::mutate(pct_other_classes = 1 - pct_govt_fi)

df_events <- data.frame(
  name = c('net_debtor','build_up','gfc','post_gfc','net_creditor'),
  start = as.Date(c(start_date,'2006-02-23','2008-03-16','2009-03-09','2012-02-29')),
  end = as.Date(c('2006-02-23','2008-03-16','2009-03-09','2012-02-29','2021-05-05')),
  cycle_id = c('net_debtor','build_up','gfc','post_gfc','net_creditor'),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(median_x = start + floor((end-start)/2))

df_saa <- data.frame(
  year = seq.Date(from = as.Date('2002-01-01'), to = as.Date('2020-01-01'), by = 'years'),
  a_govt = c(.562,.456,.525,.686,.718,.844,.785,.898,.802,.8351,.8987,.9117,.8999,.8847,.887,.8764,.9318,.9303,.8818),
  b_agcy = c(.051,.015,.032,.037,.095,.043,.071,.04,.059,.0712,.0319,.0231,.027,.0257,.016,.0161,.0156,.0178,.0245),
  c_supra = c(.028,.035,.008,.034,.018,.065,.13,.019,.018,.044,.0123,.0105,.0126,.0101,.007,.0074,.0078,.0086,.0122),
  d_td_bank = c(.348,.459,.411,.227,.154,.04,.004,.012,.012,.0033,.0041,.0027,.0024,.0016,.002,.0065,.0036,.0041,.0023),
  e_td_supra = c(0,.009,.012,.003,.002,.001,.005,.024,.102,.0383,.0407,.0347,.0369,.045,.0787,.0849,.0247,.0163,.0499),
  f_equity = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,.0074,.0104,.0129),
  g_us_mbs = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,.0014,.0015,.0015),
  h_other = c(.011,.026,.012,.013,.013,.007,.005,.007,.007,.0081,.0123,.0173,.0212,.0329,.0093,.0087,.0077,.011,.0149)
)

### GGPlots

p0 <- ggplot()

p1 <- df_bra_fx %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = values, color = ticker, fill = ticker, group = ticker), size = 1.25) +
  geom_rect(data = df_events, mapping = aes(NULL,NULL,xmin = start, xmax = end, fill = cycle_id), ymin = 0, ymax = 6, color = 'white', size = 0.5, alpha = 0.250) +
  geom_text(data = df_events, mapping = aes(x = median_x, y = 0.5, label = name), size = 4, fontface = 'bold', color = 'white') +
  labs(title = 'Brazil: FX Rate', subtitle = 'BRL per USD', caption = NULL, x = NULL) +
  scale_x_date(expand = c(0,0), date_labels = "%Y") +
  scale_y_comma(limits = c(0,6), breaks = seq(0,6,1), accuracy = 1, name = NULL, position = 'left') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2 <- df_bra_rsrv %>%
  ggplot() +
  geom_area(mapping = aes(x = date, y = values, color = ticker, fill = ticker, group = ticker), size = 1.25) +
  geom_rect(data = df_events, mapping = aes(NULL,NULL,xmin = start, xmax = end, fill = cycle_id), ymin = 0, ymax = 400000, color = 'white', size = 0.5, alpha = 0.250) +
  geom_text(data = df_events, mapping = aes(x = median_x, y = 25000, label = name), size = 4, fontface = 'bold', color = 'white') +
  labs(title = 'Brazil: FX Reserves', subtitle = 'In USD Millions', caption = NULL, x = NULL) +
  scale_x_date(expand = c(0,0), date_labels = "%Y") +
  scale_y_comma(limits = c(0,400000), breaks = seq(0,400000,50000), accuracy = 1, name = NULL, position = 'left') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p3 <- df_stats %>%
  dplyr::select(year,return) %>%
  pivot_longer(cols = -year, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_bar(mapping = aes(x = year, y = values, color = values, fill = values, group = ticker), position = "stack" , stat = "identity", color = 'white') +
  geom_text(aes(label = percent(values, accuracy = 0.01), x = year, y = values), vjust = ifelse(df_stats$return >= 0, -1, 1) * 1.375, size = 4.0, color = ifelse(df_stats$return >= 0, "darkgreen", "darkred"), fontface = "bold") +
  labs(title = 'Brazil: FX Reserves', subtitle = 'End of Period Returns (USD)', caption = NULL, x = NULL, y = NULL) +
  scale_y_percent(limits = c(-0.05,.11), breaks = seq(-.04,.1,.02), accuracy = 1) +
  scale_x_date(expand = c(0,0), date_breaks = '1 year', minor_breaks = '1 year', date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p4 <- df_stats %>%
  dplyr::select(year,duration) %>%
  pivot_longer(cols = -year, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_bar(mapping = aes(x = year, y = values, color = values, fill = values, group = ticker), position = "stack" , stat = "identity", color = 'white') +
  geom_text(aes(label = comma(values, accuracy = 0.01), x = year, y = values), vjust = ifelse(df_stats$duration >= 0, -1, 1) * 1.375, size = 4.0, color = ifelse(df_stats$duration >= 0, "darkgreen", "darkred"), fontface = "bold") +
  labs(title = 'Brazil: FX Reserves', subtitle = 'Average Duration (in years)', caption = NULL, x = NULL, y = NULL) +
  scale_y_comma(limits = c(0,3.25), breaks = seq(0,3,.5), accuracy = .1) +
  scale_x_date(expand = c(0,0), date_breaks = '2 years', minor_breaks = '1 year', date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p5 <- df_stats %>%
  dplyr::select(year,pct_usd,pct_other_ccys) %>%
  pivot_longer(cols = -year, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_bar(mapping = aes(x = year, y = values, color = ticker, fill = ticker), position = position_stack(reverse = FALSE), stat = "identity", color = 'white') +
  labs(title = 'Brazil: FX Reserves', subtitle = 'Currency Allocation', caption = NULL, x = NULL, y = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(limits = c(0,1), breaks = seq(0,1,.1), accuracy = 1) +
  scale_x_date(expand = c(0,0), date_breaks = '1 year', minor_breaks = '1 year', date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE, direction = -1) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE, direction = -1)

p6 <- df_stats %>%
  dplyr::select(year,pct_govt_fi,pct_other_classes) %>%
  pivot_longer(cols = -year, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_bar(mapping = aes(x = year, y = values, color = ticker, fill = ticker),position = position_stack(reverse = TRUE), stat = "identity", color = 'white') +
  labs(title = 'Brazil: FX Reserves', subtitle = 'Asset Class Allocation', caption = NULL, x = NULL, y = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(limits = c(0,1), breaks = seq(0,1,.1), accuracy = 1, position = 'right') +
  scale_x_date(expand = c(0,0), date_breaks = '1 year', minor_breaks = '1 year', date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE, direction = 1) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE, direction = 1)

p7 <- df_us_ylds %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'US: Government Bond Yields', subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_comma(limits = c(-2,8), breaks = seq(-2,8,2),accuracy = 1, name = NULL, position = 'left') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p8 <- df_de_ylds %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Germany: Government Bond Yields', subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_comma(limits = c(-2,8), breaks = seq(-2,8,2),accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p9 <- df_jp_ylds %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Japan: Government Bond Yields', subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_comma(limits = c(-2,8), breaks = seq(-2,8,2),accuracy = 1, name = NULL, position = 'left') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p10 <- df_uk_ylds %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'UK: Government Bond Yields', subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_comma(limits = c(-2,8), breaks = seq(-2,8,2),accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p11 <- df_fi_indx %>%
  dplyr::select(date,ticker,arets_rsum) %>%
  dplyr::arrange(date,ticker) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = arets_rsum, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Fixed Income: Index Returns', subtitle = 'Rolling 5 Year', caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p12 <- df_eq_indx %>%
  dplyr::select(date,ticker,arets_rsum) %>%
  dplyr::arrange(date,ticker) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = arets_rsum, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Equities: Index Returns', subtitle = 'Rolling 5 Year', caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p13 <- df_fi_etfs_0 %>%
  dplyr::select(date,ticker,arets_acum) %>%
  dplyr::arrange(date,ticker) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = arets_acum, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Fixed Income: Index and ETF Returns', subtitle = 'US MBS', caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p14 <- df_fi_etfs_1 %>%
  dplyr::select(date,ticker,arets_acum) %>%
  dplyr::arrange(date,ticker) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = arets_acum, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Fixed Income: Index and ETF Returns', subtitle = 'US Corporate', caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p15 <- df_fi_etfs_2 %>%
  dplyr::select(date,ticker,arets_acum) %>%
  dplyr::arrange(date,ticker) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = arets_acum, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Fixed Income: Index and ETF Returns', subtitle = 'US Aggregate', caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p16 <- df_eq_etfs %>%
  dplyr::select(date,ticker,arets_acum) %>%
  dplyr::arrange(date,ticker) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = arets_acum, color = ticker, group = ticker), size = 1.25) +
  geom_hline(yintercept = 0, color = 'gray32', size = 0.50, alpha = 0.50) +
  labs(title = 'Equities: Index and ETF Returns', subtitle = 'S&P 500', caption = NULL, x = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_y_percent(accuracy = 1, name = NULL, position = 'right') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p17 <- df_saa %>%
  dplyr::select(year,a_govt,b_agcy,c_supra,d_td_bank,e_td_supra,f_equity,g_us_mbs,h_other) %>%
  pivot_longer(cols = -year, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_bar(mapping = aes(x = year, y = values, color = ticker, fill = ticker), position = position_stack(reverse = TRUE), stat = "identity", color = 'white') +
  labs(title = 'Brazil: FX Reserves', subtitle = 'Asset Allocation', caption = NULL, x = NULL, y = NULL) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.text = element_text(size = 12)) +
  scale_y_percent(limits = c(0,1), breaks = seq(0,1,.1), accuracy = 1) +
  scale_x_date(expand = c(0,0), date_breaks = '1 year', minor_breaks = '1 year', date_labels = "%Y") +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE, direction = 1) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE, direction = 1)

# grid.arrange(p1,p2,ncol = 1, nrow = 2)

g1 <- list(p0,p1,p2)
tiles <- 100
zero_tiles <- 2
grid.arrange(
  grobs = g1,
  widths = rep(1,times = tiles),
  layout_matrix = rbind(
    c(rep(1, times = zero_tiles), rep(2, times = (tiles - zero_tiles))),
    c(rep(3, times = tiles))
  )
)

g2 <- list(p3,p4,p5,p6)
tiles <- 6
zero_tiles <- 0
grid.arrange(
  grobs = g2,
  widths = rep(1,times = tiles),
  layout_matrix = rbind(
    c(rep(1,times = 4),2,2),
    c(3,3,3,4,4,4)
  )
)

grid.arrange(p7,p8,p9,p10,ncol = 2,nrow = 2)

grid.arrange(p11,p12,ncol = 1,nrow = 2)

g3 <- list(p13,p14,p15,p16)
t_tiles <- 100
a_tiles <- 60
b_tiles <- t_tiles - a_tiles
grid.arrange(
  grobs = g3,
  widths = rep(1,times = t_tiles),
  layout_matrix = rbind(
    c(rep(1,times = a_tiles),rep(2,times = b_tiles)),
    c(rep(3,times = b_tiles),rep(4,times = a_tiles))
  )
)

