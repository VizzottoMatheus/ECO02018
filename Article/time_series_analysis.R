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

ts_plot(diesel_ts,
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

ggseasonplot(diesel_ts) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

ggseasonplot(diff(diesel_ts)) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")

ggsubseriesplot(diesel_ts) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

ggsubseriesplot(diff(diesel_ts)) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")


######### QUEBRA ESTRUTURAL

model1 <- Fstats(df_rs$VENDAS~1, from = 0.01)
sctest(model1)

breakpoints(df_rs$VENDAS~1)

quebras <- breakpoints(df_rs$VENDAS~1)
quebras <- quebras$breakpoints

# DATAS COM POSSÍVEL QUEBRA ESTRUTURAL
df_rs[quebras,1]
