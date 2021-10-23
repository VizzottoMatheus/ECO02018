
library(forecast)
library(readxl)
library(tseries)
library(ggplot2)
library(gridExtra)

#setwd("C:/Users/user/Projetos/ECO02018/04_Time_Series/markdown")
path <- "../data/ARIMA.xls"
df <- read_excel(path, sheet = "data")

# DATES
date_min <- min(df$Ano)
date_max <- max(df$Ano)

########## RPD ##########
rpd <- df$RPD
rpd_ts <- ts(rpd, start = c(date_min, 1), end = c(date_max, 4), frequency = 4)


# GRÁFICOS
autoplot(rpd_ts) + theme_bw() + labs(x = "Trimestre", y = "Valor", title = "RPD")
p1 <- ggAcf(rpd_ts) + theme_bw() + labs(title = element_blank())
p2 <- ggPacf(rpd_ts) + theme_bw() + labs(title = element_blank())
grid.arrange(p1, p2, ncol = 2, top = "RPD")

# TESTES
adf.test(rpd_ts) # não estacionária em nível
rpd_ts_diff1 <- diff(rpd_ts)
adf.test(rpd_ts_diff1) # I = 1 
adf.test(rpd_ts_diff1, k = 2) # k = número de lags incluídos no teste
pp.test(rpd_ts_diff1) # teste Phillips-Perron (corrobora ADF?)

# GRÁFICOS SÉRIE DIFERENCIADA
p1 <- ggAcf(rpd_ts_diff1) + theme_bw() + labs(title = element_blank())
p2 <- ggPacf(rpd_ts_diff1) + theme_bw() + labs(title = element_blank())
grid.arrange(p1, p2, ncol = 2, top = "RPD em primeira diferença")

### MODELOS

# AUTOARIMA
aa_ <- auto.arima(rpd_ts, D = 1, stepwise = FALSE, approximation = FALSE)
summary(aa_)

# LAG 5
arima_rpd_01 <- arima(rpd_ts, order = c(0, 1, 0), seasonal = list(order = c(1,0,1), period = 5))
summary(arima_rpd_01)
checkresiduals(arima_rpd_01) # H0 Ljung-Box: ausência de autocorrelação 


# MA 5, AR 12
arima_rpd_02 <- arima(rpd_ts, order = c(0, 1, 0), seasonal = list(order = c(1,0,1), period = 12))
summary(arima_rpd_02)
checkresiduals(arima_rpd_02) # H0 Ljung-Box: ausência de autocorrelação 



arima_rpd_01 <- arima(rpd_ts, order = c(5, 0, 5), 
                      fixed = c(0, 0, 0, 0, NA, 0, 0, 0, 0, NA, NA))


####### DPC #########
dpc <- df$DPC
dpc_ts <- ts(dpc, start = c(date_min, 1), end = c(date_max, 4), frequency = 4)

# GRÁFICOS
autoplot(dpc_ts) + theme_bw() + labs(x = "Trimestre", y = "Valor", title = "DPC")
p1 <- ggAcf(dpc_ts) + theme_bw() + labs(title = element_blank())
p2 <- ggPacf(dpc_ts) + theme_bw() + labs(title = element_blank())
grid.arrange(p1, p2, ncol = 2, top = "DPC")

# TESTES
adf.test(dpc_ts) # não estacionária em nível
dpc_ts_diff1 <- diff(dpc_ts)
adf.test(dpc_ts_diff1) 
adf.test(dpc_ts_diff1, k = 2) # k = número de lags incluídos no teste
pp.test(dpc_ts_diff1) # teste Phillips-Perron (corrobora ADF?) --> I = 1

# GRÁFICOS SÉRIE DIFERENCIADA
p1 <- ggAcf(dpc_ts_diff1) + theme_bw() + labs(title = element_blank())
p2 <- ggPacf(dpc_ts_diff1) + theme_bw() + labs(title = element_blank())
grid.arrange(p1, p2, ncol = 2, top = "RPD em primeira diferença")


