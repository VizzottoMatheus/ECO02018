library(readxl)
library(tidyverse)
library(vars)
library(tseries)
library(tsDyn)
library(fpp3)

df0 <- read_excel("data/VAR.xls")
df <- select(df0, c("DPC", "RPD"))

#######################
## ESTACIONARIEDADE ###
#######################
apply(df, 2, adf.test)

df_diff <- apply(df, 2, diff)
apply(df_diff, 2, adf.test) # k: número de lags no teste
apply(df_diff, 2, pp.test)  # I(1)

#######################
##### LAGS ÓTIMOS #####
#######################
VARselect(df_diff)

#######################
####### MODELOS #######
#######################

var_model_01 <- VAR(df_diff, p = 1, type = "none")
var_model_02 <- VAR(df_diff, p = 1, type = "const")
var_model_03 <- VAR(df_diff, p = 1, type = "trend")
var_model_04 <- VAR(df_diff, p = 1, type = "both")

var_model_01
var_model_02
var_model_03
var_model_04
 
### MODELO 1 ###
#Teste de independência dos resíduos (H0: ausência de autocorrelação serial)
serial.test(var_model_01, type = "BG")
#Teste de heterocedasticidade dos resíduos (HO: resíduos são homocedásticos)
arch.test(var_model_01)
#Teste de normalidade dos resíduos (JB-test H0: dados com distribuição normal)
normality.test(var_model_01)
# CAUSALIDADE DE GRANGER
causality(var_model_01, cause = "DPC") # DPC causa RPD
causality(var_model_01, cause = "RPD")
#Função Impulso Resposta
irf(var_model_01, impulse = "DPC", response = "RPD", n.ahead = 20) %>% 
  plot()
irf(var_model_01, cumulative = TRUE, n.ahead = 20) %>% 
  plot()

### MODELO 2 ###
#Teste de independência dos resíduos
serial.test(var_model_02, type = "BG")
#Teste de heterocedasticidade dos resíduos
arch.test(var_model_02)
#Teste de normalidade dos resíduos
normality.test(var_model_02)
# CAUSALIDADE DE GRANGER
causality(var_model_02, cause = "DPC") # DPC causa RPD
causality(var_model_02, cause = "RPD")
#Função Impulso Resposta
irf(var_model_02, impulse = "DPC", response = "RPD", n.ahead = 20) %>% 
  plot()
irf(var_model_02, cumulative = TRUE, n.ahead = 20) %>% 
  plot()

### MODELO 3 ###
#Teste de independência dos resíduos
serial.test(var_model_03, type = "BG")
#Teste de heterocedasticidade dos resíduos
arch.test(var_model_03)
#Teste de normalidade dos resíduos
normality.test(var_model_03)
# CAUSALIDADE DE GRANGER
causality(var_model_03, cause = "DPC") # DPC causa RPD
causality(var_model_03, cause = "RPD")
#Função Impulso Resposta
irf(var_model_03, impulse = "DPC", response = "RPD", n.ahead = 20) %>% 
  plot()
irf(var_model_03, cumulative = TRUE, n.ahead = 20) %>% 
  plot()

### MODELO 4 ###
#Teste de independência dos resíduos
serial.test(var_model_04, type = "BG")
#Teste de heterocedasticidade dos resíduos
arch.test(var_model_04)
#Teste de normalidade dos resíduos
normality.test(var_model_04)
# CAUSALIDADE DE GRANGER
causality(var_model_04, cause = "DPC") # DPC causa RPD
causality(var_model_04, cause = "RPD")
#Função Impulso Resposta
irf(var_model_04, impulse = "DPC", response = "RPD", n.ahead = 20) %>% 
  plot()
irf(var_model_04, cumulative = TRUE, n.ahead = 20) %>% 
  plot()


#######################
###### PREDIÇÃO #######
#######################

predicao <- predict(var_model_04 , n.ahead = 30, ci = 0.95)
plot(predicao)

