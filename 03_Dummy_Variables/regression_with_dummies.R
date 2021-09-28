library(readxl)
library(tidyr)
library(fastDummies)

#Estime um equação com a variável Salário como dependente e Anos (experiência) como variável explicativa quantitativa.
#1) Teste se há discriminação salarial pelo fato da pessoa ser mulher.
#2) Teste se há discriminação salarial pelo fato da pessoa ser preta.
#3) Interprete os valores encontrados para todos os parâmetros dos modelos (independentemente de serem significativos).

### GÊNERO ###

df <- read_excel("data.xlsx", sheet = "Gender")
df <-  df %>% 
       fill(Cidade, .direction = "down")

#df <- dummy_cols(df, select_columns = 'Gênero')
df$Genero_homem <- ifelse(df$Gênero == "h", 1, 0)

m1 <- lm(df$`Salário (em R$)` ~ df$Anos + df$Genero_homem)
summary(m1) 

# O teste de hipóteses da análise não mostrou a variável
# de gênero como significativa ao nível de 10%, ou seja:
# para estes dados hipotéticos, mulheres não sofreriam
# discriminação salarial.

# Como a variável omitida se refere às mulheres, o estimador
# do intercepto diz respeito a estas, com um salário de 
# esperado de R$ 10.241,87. Os homens teriam um acréscimo de
# R$ 655,78 neste valor como decorrência, nessa regressão,
# do seu gênero – R$ 10.897,65 esperados. Por fim, um ano de experiência
# elevaria o salário do indivíduo em R$ 310,22.





### RAÇA ###

df <- read_excel("data.xlsx", sheet = "Race")
df <-  df %>% 
  fill(Cidade, .direction = "down")
df

#df <- dummy_cols(df, select_columns = 'Raça')
df$Raca_negro <- ifelse(df$Raça == "n", 1, 0)

m1 <- lm(df$`Salário médio (R$)` ~ df$`Anos de Estudo` + df$Raca_negro)
summary(m1) # SIGNIFICATIVO, NEGROS GANHAM MENOS NA MÉDIA


#Neste segundo caso, a raça do indivíduo se mostrou significativa
#na determinação do seu salário ao nível de 1%: neste caso, há indícios
#de que pessoas negras sofram discriminação no mercado de trabalho.
#Como a variável dummy omitida se refere às pessoas de raça branca,
#o intercepto estimado de R$ 15.539,44 diz respeito ao seu salário. Negros,
#por sua vez, teriam este valor reduzido em R$ 7.007,13 (R$8.532,3 totais).
#Os anos de estudo, como o intercepto e a raça, se mostraram significativos, com
#um acréscimo de R$ 463,22 no salário por ano de atividade acadêmica.

