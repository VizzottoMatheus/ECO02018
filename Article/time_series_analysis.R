source("get_data.R")
library(forecast)
library(lubridate)
library(TSstudio)
library(ggplot2)
library(scales)
library(zoo)
library(strucchange)


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

reg_seas <- tslm(diesel_ts ~ season) # MESES SÃO SIGNIFICATIVOS?
summary(reg_seas) # JAN, MAR, ABRI, MAI, AGO, SET, OUT, NOV


######### QUEBRA ESTRUTURAL

#model1 <- Fstats(df_rs$VENDAS~1, from = 0.01)
#sctest(model1)

quebras <- breakpoints(df_rs$VENDAS~1)
quebras

# DATAS COM POSSÍVEL QUEBRA ESTRUTURAL
quebras <- quebras$breakpoints
dates <- as.Date(df_rs[quebras,1])
values <- df_rs[quebras,2]
df_bp <- as.data.frame(cbind(dates, values))
df_bp$dates <- as.Date(df_bp$dates)

ggplot(df_rs, aes(x = ANO_MES, y = VENDAS)) +
  geom_line() +
  geom_point(data = df_bp, aes(x = dates, y = values), col = "red", size = 2)


######### MODELOS


# 1
reg_seas <- tslm(diesel_ts ~ season) # MESES SÃO SIGNIFICATIVOS?
accuracy(reg_seas$fitted.values, df_rs$VENDAS)

# 2
reg_seas_tren <- tslm(diesel_ts ~ season + trend) # MESES SÃO SIGNIFICATIVOS?
accuracy(reg_seas_tren$fitted.values, df_rs$VENDAS)
