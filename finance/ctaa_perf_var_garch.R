### initial settings

rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

# setwd('g:/Diest/marcelom/R/taa/')
# setwd('d:/usr/marcelom/OneDrive/Code/R/taa/')
setwd('/home/marcelom/.local/code/r/finance/')

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

# theme_set(theme_bw())
theme_set(theme_minimal())

# locale settings
# windows
# Sys.setlocale('LC_ALL', 'English')
# Sys.setlocale('LC_TIME', 'Portuguese')
# linux
Sys.setlocale('LC_ALL', 'en_US.UTF-8')
Sys.setlocale('LC_TIME', 'pt_BR.UTF-8')

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

# df <- read_xlsx("d:/usr/marcelom/OneDrive/Code/R/taa/depinPerf.xlsx", sheet = "dataRead")
df <- read_xlsx("/home/marcelom/.local/code/r/finance/depinPerf.xlsx", sheet = "dataRead")
df$date <- lubridate::ymd(df$date)
# df <- subset(df, select = c(date,real_pct,ref_pct))

df$alpha_pct <- df$real_pct - df$ref_pct

df$real <- rep(100, nrow(df))
df$ref <- rep(100, nrow(df))
df$alpha <- rep(100, nrow(df))

df$logret_real <- log(df$real_pct + 1)
df$logret_ref <- log(df$ref_pct + 1)
df$logret_alpha <- log(df$alpha_pct + 1)

df$log_real <- rep(log(100), nrow(df))
df$log_ref <- rep(log(100), nrow(df))
df$log_alpha <- rep(log(100), nrow(df))

for (i in 2:nrow(df)) {
  df$real[i] <- df$real[i-1] *  (1 + df$real_pct[i])
  df$ref[i] <- df$ref[i-1] *  (1 + df$ref_pct[i])
  df$alpha[i] <- df$alpha[1] * (1 + ((df$real[i]/df$real[1]) - (df$ref[i]/df$ref[1])))
  df$log_real[i] <- log(df$real[i])
  df$log_ref[i] <- log(df$ref[i])
  df$log_alpha[i] <- log(df$alpha[i])
}

df$real_acum <- rep(0, nrow(df))
df$ref_acum <- rep(0, nrow(df))
df$alpha_acum <- rep(0, nrow(df))

df$real_acum <- (df$real/df$real[1]) - 1
df$ref_acum <- (df$ref/df$ref[1]) - 1
df$alpha_acum <- ((df$alpha/df$alpha[1]) - 1) * 10000

# df_ctaa_dates <- read_xlsx("d:/usr/marcelom/OneDrive/Code/R/taa/depinPerf.xlsx", sheet = "dataRead_dates")
df_ctaa_dates <- read_xlsx("/home/marcelom/.local/code/r/finance/depinPerf.xlsx", sheet = "dataRead_dates")
df_ctaa_dates$date <- lubridate::ymd(df_ctaa_dates$date)

for (Di in 0:8) {
  name_1 <- paste("ctaa", "date", "m", Di, sep = "_")
  assign(name_1, as.Date(as.numeric(df_ctaa_dates[nrow(df_ctaa_dates) - Di,1])))
}

cl <- 0.95
qtl <- 1 - cl
mgri_y <- 0.01
mgri_q <- mgri_y / sqrt(4)
mgri_m <- mgri_y / sqrt(12)
mgri_w <- mgri_y / sqrt(52)
mgri_d <- mgri_y / sqrt(252)

###

p1_log <- df %>%
  dplyr::select(date,log_real,log_ref) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1) +
  labs(title = "Reservas: Retorno Acumulado (Log)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(trans = 'log') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2_log <- df %>%
  dplyr::select(date,log_alpha) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1) +
  labs(title = "Gest?o Ativa: Resultado Acumulado (Log)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(trans = 'log') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p1_acum <- df %>%
  dplyr::select(date,real_acum,ref_acum) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1) +
  geom_text(data = filter(df, date == as.Date(tail(df$date,1))), aes(label = percent(real_acum, accuracy = 0.1), x = date, y = real_acum), position = position_dodge(.9), hjust = -0.015625, size = 3.875, color = "green4", fontface = "bold") +
  geom_text(data = filter(df, date == as.Date(tail(df$date,1))), aes(label = percent(ref_acum, accuracy = 0.1), x = date, y = ref_acum), position = position_dodge(.9), hjust = -0.015625, size = 3.875, color = "red4", fontface = "bold") +
  labs(title = "Reservas: Retorno Acumulado (%)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(date_labels = "%Y") +
  scale_y_percent(limits = c(-.1,.9), breaks = seq(0,0.8,0.2), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2_acum <- df %>%
  dplyr::select(date,alpha_acum) %>%
  pivot_longer(cols = -date, names_to = 'ticker', values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = ticker, group = ticker), size = 1) +
  geom_text(data = filter(df, date == as.Date(tail(df$date,1))), aes(label = comma(alpha_acum, accuracy = 0.1), x = date, y = alpha_acum), position = position_dodge(.9), hjust = -0.015625, size = 3.875, color = "green4", fontface = "bold") +
  labs(title = "Gest?o Ativa: Resultado Acumulado (bps)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(date_labels = "%Y") +
  scale_y_comma(limits = c(-50,350), breaks = seq(0,300,100), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

# grid.arrange(p1_log,p2_log,ncol=1,nrow=2)
grid.arrange(p1_acum,p2_acum,ncol=1,nrow=2)

###

df_xts <- as.xts(df, order.by = df$date)
df_xts <- subset(df_xts, select = c(real,ref)) %>% na.omit()
storage.mode(df_xts) <- 'double'

df_logret <- diff(log(df_xts))[-1]
logret_d <- apply.daily(df_logret, colSums)
logret_w <- apply.weekly(df_logret, colSums)
logret_m <- apply.monthly(df_logret, colSums)
logret_q <- apply.quarterly(df_logret, colSums)
logret_y <- apply.yearly(df_logret, colSums)

ret_d <- exp(logret_d) - 1
ret_w <- exp(logret_w) - 1
ret_m <- exp(logret_m) - 1
ret_q <- exp(logret_q) - 1
ret_y <- exp(logret_y) - 1

ret_d$alpha <- (ret_d$real - ret_d$ref) * 10000
ret_w$alpha <- (ret_w$real - ret_w$ref) * 10000
ret_m$alpha <- (ret_m$real - ret_m$ref) * 10000
ret_q$alpha <- (ret_q$real - ret_q$ref) * 10000
ret_y$alpha <- (ret_y$real - ret_y$ref) * 10000


p1_y <- ret_y %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_y), unit = 'year') - days(1)) %>%
  dplyr::select(c(date,ref)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256) +
  geom_text(aes(label = percent(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_y$ref >= 0, -1, 1) * 1.375, size = 4.125, color = ifelse(ret_y$ref >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_y$ref) + sd(ret_y$ref), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_y$ref), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_y$ref) - sd(ret_y$ref), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('1999-12-31'), y = mean(ret_y$ref), label = percent(mean(ret_y$ref), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_y$ref) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_y$ref, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('1999-12-31'), y = as.numeric(quantile(ret_y$ref, qtl)), label = percent(as.numeric(quantile(ret_y$ref, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_y$ref, qtl))>=0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = mean(ret_y$ref[ret_y$ref < as.numeric(quantile(ret_y$ref, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('1999-12-31'), y = mean(ret_y$ref[ret_y$ref < as.numeric(quantile(ret_y$ref, qtl))]), label = percent(mean(ret_y$ref[ret_y$ref < as.numeric(quantile(ret_y$ref, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_y$ref[ret_y$ref < as.numeric(quantile(ret_y$ref, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Anuais (%)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.075,0.125), breaks = seq(-0.05,0.1,0.05), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


p2_y <- ret_y %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_y), unit = 'year') - days(1)) %>%
  dplyr::select(c(date,alpha)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256) +
  geom_text(aes(label = comma(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_y$alpha >= 0, -1, 1) * 1.375, size = 4.125, color = ifelse(ret_y$alpha >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_y$alpha) + sd(ret_y$alpha), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_y$alpha), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_y$alpha) - sd(ret_y$alpha), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('1999-12-31'), y = mean(ret_y$alpha), label = comma(mean(ret_y$alpha), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_y$alpha) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_y$alpha, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('1999-12-31'), y = as.numeric(quantile(ret_y$alpha, qtl)), label = comma(as.numeric(quantile(ret_y$alpha, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_y$alpha, qtl))>=0,'green4','red4'), fontface = "bold") +
  # annotate("text", x = as.Date('1999-12-31') + weeks(78), y = as.numeric(quantile(ret_y$alpha, qtl)), label = percent(abs(as.numeric(quantile(ret_y$alpha, qtl)) / 10000) / mgri_y, accuracy = 0.001), size = 4.125, color = 'green4', fontface = "bold") +
  geom_hline(yintercept = mean(ret_y$alpha[ret_y$alpha < as.numeric(quantile(ret_y$alpha, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('1999-12-31'), y = mean(ret_y$alpha[ret_y$alpha < as.numeric(quantile(ret_y$alpha, qtl))]), label = comma(mean(ret_y$alpha[ret_y$alpha < as.numeric(quantile(ret_y$alpha, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_y$alpha[ret_y$alpha < as.numeric(quantile(ret_y$alpha, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Anuais (bps)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-30,70), breaks = seq(-20,60,20), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p1_q <- ret_q %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_q), unit = 'quarter') - days(1)) %>%
  dplyr::select(c(date,ref)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/4) +
  geom_text(aes(label = percent(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_q$ref >= 0, -1, 1) * 1.375, size = 2.125, color = ifelse(ret_q$ref >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_q$ref) + sd(ret_q$ref), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_q$ref), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_q$ref) - sd(ret_q$ref), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_q$ref), label = percent(mean(ret_q$ref), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_q$ref) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_q$ref, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_q$ref, qtl)), label = percent(as.numeric(quantile(ret_q$ref, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_q$ref, qtl))>=0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = mean(ret_q$ref[ret_q$ref < as.numeric(quantile(ret_q$ref, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_q$ref[ret_q$ref < as.numeric(quantile(ret_q$ref, qtl))]), label = percent(mean(ret_q$ref[ret_q$ref < as.numeric(quantile(ret_q$ref, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_q$ref[ret_q$ref < as.numeric(quantile(ret_q$ref, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Trimestrais (%)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.03,0.07), breaks = seq(-.02,0.06,.02), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


p2_q <- ret_q %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_q), unit = 'quarter') - days(1)) %>%
  dplyr::select(c(date,alpha)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/4) +
  geom_text(aes(label = comma(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_q$alpha >= 0, -1, 1) * 1.375, size = 2.125, color = ifelse(ret_q$alpha >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_q$alpha) + sd(ret_q$alpha), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_q$alpha), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_q$alpha) - sd(ret_q$alpha), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_q$alpha), label = comma(mean(ret_q$alpha), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_q$alpha) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_q$alpha, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_q$alpha, qtl)), label = comma(as.numeric(quantile(ret_q$alpha, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_q$alpha, qtl))>=0,'green4','red4'), fontface = "bold") +
  # annotate("text", x = as.Date('2000-12-31') + weeks(104), y = as.numeric(quantile(ret_q$alpha, qtl)), label = percent(abs(as.numeric(quantile(ret_q$alpha, qtl)) / 10000) / mgri_q, accuracy = 0.001), size = 4.125, color = 'green4', fontface = "bold") +
  geom_hline(yintercept = mean(ret_q$alpha[ret_q$alpha < as.numeric(quantile(ret_q$alpha, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_q$alpha[ret_q$alpha < as.numeric(quantile(ret_q$alpha, qtl))]), label = comma(mean(ret_q$alpha[ret_q$alpha < as.numeric(quantile(ret_q$alpha, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_q$alpha[ret_q$alpha < as.numeric(quantile(ret_q$alpha, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Trimestrais (bps)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-25,35), breaks = seq(-20,30,10), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

p1_m <- ret_m %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_m), unit = 'month') - days(1)) %>%
  dplyr::select(c(date,ref)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/8) +
  # geom_text(aes(label = percent(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_m$ref >= 0, -1, 1) * 1.375, size = 1.875, color = ifelse(ret_m$ref >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_m$ref) + sd(ret_m$ref), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_m$ref), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_m$ref) - sd(ret_m$ref), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_m$ref), label = percent(mean(ret_m$ref), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_m$ref) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_m$ref, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_m$ref, qtl)), label = percent(as.numeric(quantile(ret_m$ref, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_m$ref, qtl))>=0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = mean(ret_m$ref[ret_m$ref < as.numeric(quantile(ret_m$ref, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_m$ref[ret_m$ref < as.numeric(quantile(ret_m$ref, qtl))]), label = percent(mean(ret_m$ref[ret_m$ref < as.numeric(quantile(ret_m$ref, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_m$ref[ret_m$ref < as.numeric(quantile(ret_m$ref, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Mensais (%)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.025,0.035), breaks = seq(-0.02,0.03,0.01),accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


p2_m <- ret_m %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_m), unit = 'month') - days(1)) %>%
  dplyr::select(c(date,alpha)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/8) +
  # geom_text(aes(label = comma(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_m$alpha >= 0, -1, 1) * 1.375, size = 1.875, color = ifelse(ret_m$alpha >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_m$alpha) + sd(ret_m$alpha), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_m$alpha), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_m$alpha) - sd(ret_m$alpha), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_m$alpha), label = comma(mean(ret_m$alpha), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_m$alpha) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_m$alpha, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_m$alpha, qtl)), label = comma(as.numeric(quantile(ret_m$alpha, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_m$alpha, qtl))>=0,'green4','red4'), fontface = "bold") +
  # annotate("text", x = as.Date('2000-12-31') + weeks(104), y = as.numeric(quantile(ret_m$alpha, qtl)), label = percent(abs(as.numeric(quantile(ret_m$alpha, qtl)) / 10000) / mgri_m, accuracy = 0.001), size = 4.125, color = 'green4', fontface = "bold") +
  geom_hline(yintercept = mean(ret_m$alpha[ret_m$alpha < as.numeric(quantile(ret_m$alpha, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_m$alpha[ret_m$alpha < as.numeric(quantile(ret_m$alpha, qtl))]), label = comma(mean(ret_m$alpha[ret_m$alpha < as.numeric(quantile(ret_m$alpha, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_m$alpha[ret_m$alpha < as.numeric(quantile(ret_m$alpha, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Mensais (bps)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-22.5,22.5), breaks = seq(-20,20,5), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


p1_w <- ret_w %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_w), unit = 'week') - days(1)) %>%
  dplyr::select(c(date,ref)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/16) +
  # geom_text(aes(label = percent(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_w$ref >= 0, -1, 1) * 1.375, size = 1.875, color = ifelse(ret_w$ref >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_w$ref) + sd(ret_w$ref), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_w$ref), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_w$ref) - sd(ret_w$ref), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_w$ref), label = percent(mean(ret_w$ref), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_w$ref) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_w$ref, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_w$ref, qtl)), label = percent(as.numeric(quantile(ret_w$ref, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_w$ref, qtl))>=0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = mean(ret_w$ref[ret_w$ref < as.numeric(quantile(ret_w$ref, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_w$ref[ret_w$ref < as.numeric(quantile(ret_w$ref, qtl))]), label = percent(mean(ret_w$ref[ret_w$ref < as.numeric(quantile(ret_w$ref, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_w$ref[ret_w$ref < as.numeric(quantile(ret_w$ref, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Semanais (%)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.0175,0.0175), breaks = seq(-0.015,0.015,0.005),accuracy = 0.1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


p2_w <- ret_w %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_w), unit = 'week') - days(1)) %>%
  dplyr::select(c(date,alpha)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/16) +
  # geom_text(aes(label = comma(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_w$alpha >= 0, -1, 1) * 1.375, size = 1.875, color = ifelse(ret_w$alpha >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_w$alpha) + sd(ret_w$alpha), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_w$alpha), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_w$alpha) - sd(ret_w$alpha), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_w$alpha), label = comma(mean(ret_w$alpha), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_w$alpha) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_w$alpha, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_w$alpha, qtl)), label = comma(as.numeric(quantile(ret_w$alpha, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_w$alpha, qtl))>=0,'green4','red4'), fontface = "bold") +
  annotate("text", x = as.Date('2000-12-31') + weeks(156), y = as.numeric(quantile(ret_w$alpha, qtl)), label = percent(abs(as.numeric(quantile(ret_w$alpha, qtl)) / 10000) / mgri_w, accuracy = 0.001), size = 4.125, color = 'green4', fontface = "bold") +
  geom_hline(yintercept = mean(ret_w$alpha[ret_w$alpha < as.numeric(quantile(ret_w$alpha, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_w$alpha[ret_w$alpha < as.numeric(quantile(ret_w$alpha, qtl))]), label = comma(mean(ret_w$alpha[ret_w$alpha < as.numeric(quantile(ret_w$alpha, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_w$alpha[ret_w$alpha < as.numeric(quantile(ret_w$alpha, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Semanais (bps)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-17.5,17.5), breaks = seq(-15,15,5), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


p1_d <- ret_d %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_d), unit = 'day') - days(1)) %>%
  dplyr::select(c(date,ref)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/32) +
  # geom_text(aes(label = percent(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_d$ref >= 0, -1, 1) * 1.375, size = 1.875, color = ifelse(ret_d$ref >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_d$ref) + sd(ret_d$ref), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_d$ref), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_d$ref) - sd(ret_d$ref), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_d$ref), label = percent(mean(ret_d$ref), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_d$ref) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_d$ref, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_d$ref, qtl)), label = percent(as.numeric(quantile(ret_d$ref, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_d$ref, qtl))>=0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = mean(ret_d$ref[ret_d$ref < as.numeric(quantile(ret_d$ref, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_d$ref[ret_d$ref < as.numeric(quantile(ret_d$ref, qtl))]), label = percent(mean(ret_d$ref[ret_d$ref < as.numeric(quantile(ret_d$ref, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_d$ref[ret_d$ref < as.numeric(quantile(ret_d$ref, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Carteira de Refer?ncia: Retornos Di?rios (%)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.01375,0.01375), breaks = seq(-0.0125,0.0125,0.0025),accuracy = 0.01) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)


p2_d <- ret_d %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_d), unit = 'day') - days(1)) %>%
  dplyr::select(c(date,alpha)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = returns, color = returns, fill = returns), position = "stack" , stat = "identity", width = 256/32) +
  # geom_text(aes(label = comma(returns, accuracy = 0.1), x = date, y = returns), vjust = ifelse(ret_d$alpha >= 0, -1, 1) * 1.375, size = 1.875, color = ifelse(ret_d$alpha >= 0, "darkgreen", "darkred"), fontface = "bold") +
  geom_hline(yintercept = mean(ret_d$alpha) + sd(ret_d$alpha), size = 28/64, color = "black", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_d$alpha), size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_hline(yintercept = mean(ret_d$alpha) - sd(ret_d$alpha), size = 28/64, color = "black", alpha = 0.25) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_d$alpha), label = comma(mean(ret_d$alpha), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_d$alpha) >= 0,'green4','red4'), fontface = "bold") +
  geom_hline(yintercept = as.numeric(quantile(ret_d$alpha, qtl)), size = 0.75, color = "darkorange4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = as.numeric(quantile(ret_d$alpha, qtl)), label = comma(as.numeric(quantile(ret_d$alpha, qtl)), accuracy = 0.001), size = 4.125, color = ifelse(as.numeric(quantile(ret_d$alpha, qtl))>=0,'green4','red4'), fontface = "bold") +
  annotate("text", x = as.Date('2000-12-31') + weeks(156), y = as.numeric(quantile(ret_d$alpha, qtl)), label = percent(abs(as.numeric(quantile(ret_d$alpha, qtl)) / 10000) / mgri_d, accuracy = 0.001), size = 4.125, color = 'green4', fontface = "bold") +
  geom_hline(yintercept = mean(ret_d$alpha[ret_d$alpha < as.numeric(quantile(ret_d$alpha, qtl))]), size = 0.75, color = "magenta4", alpha = 0.50) +
  annotate("text", x = as.Date('2000-12-31'), y = mean(ret_d$alpha[ret_d$alpha < as.numeric(quantile(ret_d$alpha, qtl))]), label = comma(mean(ret_d$alpha[ret_d$alpha < as.numeric(quantile(ret_d$alpha, qtl))]), accuracy = 0.001), size = 4.125, color = ifelse(mean(ret_d$alpha[ret_d$alpha < as.numeric(quantile(ret_d$alpha, qtl))])>=0,'green4','red4'), fontface = "bold") +
  labs(title = "Gest?o Ativa: Retornos Di?rios (bps)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_comma(limits = c(-12.5,12.5), breaks = seq(-10,10,5), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha)

# histograma retornos

bins_s <- ((2^6)/2)+1 # numero de gavetas para o histograma (33)
bins_m <- ((2^7)/2)+1 # numero de gavetas para o histograma (65)
bins_l <- ((2^8)/2)+1 # numero de gavetas para o histograma (129)


p3_w <- ret_w %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_w), unit = 'week') - days(1)) %>%
  dplyr::select(c(date,ref)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_histogram(mapping = aes(x = returns, color = portfolio, fill = portfolio), bins = bins_m) +
  geom_vline(aes(xintercept = 0), size = 0.50, alpha = 0.50, color = 'black') +
  labs(title = "Carteira de Refer?ncia: Histograma Retornos Semanais (%)", subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_percent(accuracy = 0.1, name = 'Retornos') +
  scale_y_comma(accuracy = 1, name = 'Frequ?ncia') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha/2, discrete = TRUE)

p4_w <- ret_w %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_w), unit = 'day') - days(1)) %>%
  dplyr::select(c(date,alpha)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_histogram(mapping = aes(x = returns, color = portfolio, fill = portfolio), bins = bins_m) +
  geom_vline(aes(xintercept = 0), size = 0.50, alpha = 0.50, color = 'black') +
  labs(title = "Gest?o Ativa: Histograma Retornos Semanais (%)", subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_comma(accuracy = 1, name = 'Retornos') +
  scale_y_comma(accuracy = 1, name = 'Frequ?ncia') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha/2, discrete = TRUE)


p3_d <- ret_d %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_d), unit = 'day') - days(1)) %>%
  dplyr::select(c(date,ref)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_histogram(mapping = aes(x = returns, color = portfolio, fill = portfolio), bins = bins_l) +
  geom_vline(aes(xintercept = 0), size = 0.50, alpha = 0.50, color = 'black') +
  labs(title = "Carteira de Refer?ncia: Histograma Retornos Di?rios (%)", subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_percent(accuracy = 0.1, name = 'Retornos') +
  scale_y_comma(accuracy = 1, name = 'Frequ?ncia') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha/2, discrete = TRUE)

p4_d <- ret_d %>%
  as.data.frame() %>%
  mutate(date = ceiling_date(index(ret_d), unit = 'day') - days(1)) %>%
  dplyr::select(c(date,alpha)) %>%
  pivot_longer(cols = -date,
               names_to = 'portfolio',
               values_to = 'returns') %>%
  ggplot() +
  geom_histogram(mapping = aes(x = returns, color = portfolio, fill = portfolio), bins = bins_l) +
  geom_vline(aes(xintercept = 0), size = 0.50, alpha = 0.50, color = 'black') +
  labs(title = "Gest?o Ativa: Histograma Retornos Di?rios (bps)", subtitle = NULL, caption = NULL, x = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_comma(accuracy = 1, name = 'Retornos') +
  scale_y_comma(accuracy = 1, name = 'Frequ?ncia') +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha/2, discrete = TRUE)


grid.arrange(p1_y,p2_y,ncol=1,nrow=2)
grid.arrange(p1_q,p2_q,ncol=1,nrow=2)
grid.arrange(p1_m,p2_m,ncol=1,nrow=2)
grid.arrange(p1_w,p2_w,ncol=1,nrow=2)
grid.arrange(p3_w,p4_w,ncol=1,nrow=2)
grid.arrange(p1_d,p2_d,ncol=1,nrow=2)
grid.arrange(p3_d,p4_d,ncol=1,nrow=2)

logret <- log(1+ (ret_d$alpha / 10000)) %>% na.omit()

# logret <- log_alpha_d['2019-01-01/2021-02-03']

mu <- mean(logret)
sig <- sd(logret)

garch.T <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                      distribution.model = 'std')

fit.garch.T <- ugarchfit(spec = garch.T, data = logret)

save_1 <- cbind(logret, fit.garch.T@fit$sigma, fit.garch.T@fit$z)
names(save_1) <- c('logret', 's', 'z')
parm_1 <- fit.garch.T@fit$coef

sims <- 100000

boot.garch <- ugarchboot(fit.garch.T,
                         method = 'Partial',
                         sampling = 'raw',      # bootstrap from fitted epsilon
                         n.ahead = 1,           # simulation horizon
                         n.bootpred = sims,     # number of simulations
                         solver = 'solnp')

rvec <- boot.garch@fseries                      # vector of simulated outcomes
VaR <- quantile(rvec,qtl)
ES <- mean(rvec[rvec<VaR])

cutoff_date_week <- floor_date(Sys.Date() - weeks(2*52), unit = 'week')
cutoff_date_month <- ceiling_date(floor_date(Sys.Date() - weeks(2*52), unit = 'month') - days(1), unit = 'week')

n_ctaa <- length(logret['2001-07-03/2019-02-03'])

roll.garch <- ugarchroll(spec = garch.T,
                         data = logret,
                         n.ahead = 1,
                         forecast.length = 1,
                         n.start = n_ctaa,
                         refit.every = 1,
                         refit.window = 'recursive',
                         calculate.VaR = TRUE,
                         VaR.alpha = qtl,
                         keep.coef = TRUE)

ts_rollgarch <- roll.garch@forecast$VaR %>%
  as.xts()

df_rollgarch <- as.data.frame(ts_rollgarch)
df_rollgarch$date <- as.Date(index(ts_rollgarch))
colnames(df_rollgarch) <- c('alpha_5pct','realized','date')

df_rollgarch$count <- ifelse(df_rollgarch$realized < df_rollgarch$alpha_5pct,1,0)
actual_events <- sum(df_rollgarch$count)/dim(df_rollgarch)[1]

p1_var <- df_rollgarch %>%
  dplyr::select(date, alpha_5pct) %>%
  pivot_longer(cols = -date,
               names_to = 'idx',
               values_to = 'values') %>%
  ggplot() +
  geom_bar(data = df_rollgarch, mapping = aes(x = date, y = realized), width = 256/256, position="stack", stat="identity", color = ifelse(df_rollgarch$realized >= 0, 'springgreen4', 'red3'), fill = ifelse(df_rollgarch$realized >= 0, 'springgreen4', 'red3')) +
  geom_line(mapping = aes(x = date, y = values, color = idx, group = idx), size = 1.250) +
  geom_vline(xintercept = ctaa_date_m_0, size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_vline(xintercept = ctaa_date_m_1, size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_vline(xintercept = ctaa_date_m_2, size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_vline(xintercept = ctaa_date_m_3, size = 56/64, color = "orangered4", alpha = 0.25) +
  annotate("text", x = ceiling_date(cutoff_date_week + weeks(1), unit = 'week'), y = -0.00005, label = percent(actual_events, accuracy = 0.001), size = 4.125, color = 'springgreen4', fontface = "bold") +
  labs(title = "Gest?o Ativa: Resultados vs. VaR de 1 Dia (%)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_y_percent(limits = c(-0.00015,0.00015), breaks = seq(-0.0001,0.0001,0.0001), accuracy = 0.01) +
  scale_x_date(date_breaks = '3 months', date_labels = "%b %y", name = NULL) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

p2_var <- df_rollgarch %>%
  dplyr::mutate(alpha_mgri = abs(df_rollgarch$alpha_5pct / mgri_d)) %>%
  dplyr::mutate(alpha_mgri_mavg_st = mavg(alpha_mgri,7)) %>%
  dplyr::mutate(alpha_mgri_mavg_lt = mavg(alpha_mgri,28)) %>%
  dplyr::select(date,alpha_mgri,alpha_mgri_mavg_st,alpha_mgri_mavg_lt) %>%
  pivot_longer(cols = -date,
               names_to = 'idx',
               values_to = 'values') %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = values, color = idx, group = idx), size = 1.125, alpha = 0.75) +
  geom_vline(xintercept = ctaa_date_m_0, size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_vline(xintercept = ctaa_date_m_1, size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_vline(xintercept = ctaa_date_m_2, size = 56/64, color = "orangered4", alpha = 0.25) +
  geom_vline(xintercept = ctaa_date_m_3, size = 56/64, color = "orangered4", alpha = 0.25) +
  labs(title = "Gest?o Ativa: Utiliza??o VaR de 1 Dia (% MGRI)", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(face = "bold"), plot.caption = element_text(face = "bold")) +
  scale_x_date(date_breaks = '3 months', date_labels = "%b %y", name = NULL) +
  scale_y_percent(limits = c(0.01,0.11), breaks = seq(0.02,0.10,0.02), accuracy = 1) +
  scale_color_viridis(option = v_color, alpha = v_c_alpha, discrete = TRUE) +
  scale_fill_viridis(option = v_color, alpha = v_f_alpha, discrete = TRUE)

grid.arrange(p1_var,p2_var,ncol=1,nrow=2)

percent(as.numeric(VaR), accuracy = 0.000001)
percent(as.numeric(VaR/mgri_d), accuracy = 0.000001)
percent(as.numeric(ES), accuracy = 0.000001)

