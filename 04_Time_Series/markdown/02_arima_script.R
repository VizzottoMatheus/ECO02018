
library(forecast)
library(readxl)
library(timeSeries)
library(ggplot2)

path <- "../data/ARIMA.xls"
df <- read_excel(path, sheet = "data")

# SUBSETS
rpd <- df$RPD
dpc <- df$DPC

# DATES
date_min <- min(df$Ano)
date_max <- max(df$Ano)

# TS OBJECTS
rpd_ts <- ts(rpd, start = c(date_min, 1), end = c(date_max, 4), frequency = 4)
dpc_ts <- ts(dpc, start = c(date_min, 1), end = c(date_max, 4), frequency = 4)

# CHARTS
autoplot(rpd_ts) + theme_bw() + labs(x = "Trimestre", y = "Valor", title = "RPD")
autoplot(dpc_ts) + theme_bw() + labs(x = "Trimestre", y = "Valor", title = "DPC")

### STATIONARITY TEST

# RPD
ggAcf(rpd_ts) + theme_bw() + labs(title = "RPD")
adf.test(rpd_ts)
adf.test(diff(rpd_ts)) # I = 1
ggAcf(diff(rpd_ts)) + theme_bw() + labs(title = "RPD em primeira diferença")
ggPacf(diff(rpd_ts)) + theme_bw() + labs(title = "RPD em primeira diferença")

# DPC
ggAcf(dpc_ts) + theme_bw() + labs(title = "DPC")
adf.test(dpc_ts)
ggAcf(diff(dpc_ts)) + theme_bw() + labs(title = "DPC em primeira diferença")
adf.test(diff(dpc_ts))
adf.test(diff(diff(dpc_ts))) # I = 2
ggAcf(diff(diff(dpc_ts))) + theme_bw() + labs(title = "RPD")
ggPacf(diff(diff(dpc_ts))) + theme_bw() + labs(title = "RPD")


### MODELS
aa_ <- auto.arima(rpd_ts, stepwise = FALSE, approximation = FALSE)
summary(aa_)

arima_rpd_01 <- Arima(rpd_ts, order = c(5, 1, 5))
plot(rpd_ts)
lines(fitted(arima_rpd_01), col = "red")
summary(arima_rpd_01)
checkresiduals(arima_rpd_01)


arima_rpd_01 <- arima(rpd_ts, order = c(5, 0, 5), 
                      fixed = c(0, 0, 0, 0, NA, 0, 0, 0, 0, NA, NA))

plot(rpd_ts)
lines(fitted(arima_rpd_01), col = "red")
summary(arima_rpd_01)
