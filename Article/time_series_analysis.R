source("get_data.R")
library(forecast)
library(lubridate)
library(TSstudio)
library(ggplot2)
library(scales)
library(zoo)
library(strucchange)
library(tseries)


df_cidades <- oil_data_cities()
df_rs <- oil_data_state()

head(df_rs)
tail(df_rs)

### OBJETO TIME SERIES

date_min_y <- year(min(df_rs$ANO_MES))
date_min_m <- month(min(df_rs$ANO_MES))
date_min <- c(date_min_y, date_min_m)

date_max_y <- year(max(df_rs$ANO_MES))
date_max_m <- month(max(df_rs$ANO_MES))
date_max <- c(date_max_y, date_max_m)

diesel_ts <- ts(df_rs$VENDAS, start = date_min, end = date_max, frequency = 12)
diesel_ts_log <- ts(log(df_rs$VENDAS), start = date_min, end = date_max, frequency = 12)
ts_info(diesel_ts)

### VISUALIZAÇÃO INICIAL

# SÉRIE EM NÍVEL
ts_plot(diesel_ts,
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")
# LOGARITMO DA SÉRIE EM NÍVEL (CORRIGE TENDÊNCIA NÃO-LINEAR)
ts_plot(log(diesel_ts),
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

trend <- rollapply(diesel_ts, width = 12, FUN = mean)
df <- cbind(diesel_ts, trend)
colnames(df) <- c("VENDAS", "MÉDIA MÓVEL 12 MESES")
ts_plot(df,
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

# SÉRIE EM LOGARITMO
ts_plot(log(df),
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

### DECOMPOSIÇÃO

dec <- decompose(diesel_ts)
plot(dec)

### ANÁLISE DE SAZONALIDADE

ggseasonplot(diesel_ts, continuous = TRUE) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

ggsubseriesplot(diesel_ts) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

diesel_ts_diff <- diff(diesel_ts) # cria série diferenciada

ggseasonplot(diesel_ts_diff) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")

ggsubseriesplot(diesel_ts_diff) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")

reg_seas <- tslm(diesel_ts_log ~ season) # MESES SÃO SIGNIFICATIVOS?
summary(reg_seas) # JAN, MAR, ABRI, MAI, AGO, SET, OUT, NOV


######### QUEBRA ESTRUTURAL
#model1 <- Fstats(df_rs$VENDAS~1, from = 0.01)
#sctest(model1)
#quebras <- breakpoints(df_rs$VENDAS~1)
#quebras
# DATAS COM POSSÍVEL QUEBRA ESTRUTURAL
#quebras <- quebras$breakpoints
#dates <- as.Date(df_rs[quebras,1])
#values <- df_rs[quebras,2]
#df_bp <- as.data.frame(cbind(dates, values))
#df_bp$dates <- as.Date(df_bp$dates)
#ggplot(df_rs, aes(x = ANO_MES, y = VENDAS)) +
#  geom_line() +
#  geom_point(data = df_bp, aes(x = dates, y = values), col = "red", size = 2)
######### MODELOS


# 1 - SAZONAL COM TENDÊNCIA SIMPLES
reg_seas_tren <- tslm(diesel_ts ~ season + trend) # MESES SÃO SIGNIFICATIVOS?
accuracy(reg_seas_tren$fitted.values, df_rs$VENDAS)

plot(diesel_ts, type = "l")
lines(reg_seas_tren$fitted.values, col = "red")


# 2 - SAZONAL COM TENDÊNCIA SIMPLES EM SÉRIE COM LOGARITMO
reg_seas_tren <- tslm(diesel_ts_log ~ season + trend) # MESES SÃO SIGNIFICATIVOS?
accuracy(reg_seas_tren$fitted.values, df_rs$VENDAS)

plot(diesel_ts_log, type = "l")
lines(reg_seas_tren$fitted.values, col = "red")

# 3 - ARIMA (6, 1, 3)
ggAcf(diesel_ts)
ggPacf(diesel_ts)
adf.test(diesel_ts) # presença de raiz unitária

ggAcf(diesel_ts_diff)
ggPacf(diesel_ts_diff)
adf.test(diesel_ts_diff) # ausência de raiz unitária

arima1 <- Arima(diesel_ts, order = c(6, 1, 3), method = "ML")
checkresiduals(arima1)
accuracy(arima1$fitted, diesel_ts)

plot(diesel_ts)
lines(arima1$fitted, col = "red")

# 4 - SARIMA (6, 1, 3) (0, 0, 1)
arima2 <- Arima(diesel_ts, order = c(6, 1, 3), seasonal = list(order = c(0, 0, 1)))
checkresiduals(arima2)
accuracy(arima2$fitted, diesel_ts)

plot(diesel_ts)
lines(arima2$fitted, col = "red")

# 5 - SARIMA (6, 1, 1) (5, 1, 1) SÉRIE APARENTA DECAIMENTO SAZONAL, MELHOR MODELO ATÉ AGORA
ggAcf(diesel_ts_diff, lag.max = 48)

diesel_ts_diff_seas <- diff(diesel_ts_diff, 12)
ggAcf(diesel_ts_diff_seas, lag.max = 48)
ggPacf(diesel_ts_diff_seas, lag.max = 48)


arima3 <- Arima(diesel_ts, order = c(6, 1, 1), seasonal = list(order = c(1, 1, 1)))
checkresiduals(arima3)
accuracy(arima3$fitted, diesel_ts)

plot(diesel_ts)
lines(arima3$fitted, col = "red")

# HOLT WINTERS
deep_grid <- ts_grid(diesel_ts,
                     model = "HoltWinters",
                     periods = 6,
                     window_space = 6,
                     window_test = 12,
                     hyper_params = list(alpha = seq(0.1,0.5,0.01),
                                         beta = seq(0,0.1,0.01),
                                         gamma = seq(0.2,0.4,0.01)),
                     parallel = TRUE,
                     n.cores = 8)
