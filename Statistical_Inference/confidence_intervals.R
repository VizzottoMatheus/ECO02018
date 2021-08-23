library(tigerstats)

# FUNÇÃO QUE ENCONTRA OS LIMITES INFERIOR E SUPERIOR

interval <- function(avg, ci, sd, n) {
  z2 <- ci + ((1 - ci)/2)     # nível de confiança + alfa/2 (% à esquerda do intervalo) para encontrar o percentil e z correspondente
  z <- qnorm(z2)
  error <- z*(sd/sqrt(n))
  sup <- avg + error
  inf <- avg - error
  print(paste0("Limite inferior: ", round(inf,2)))
  print(paste0("Limite superior: ", round(sup,2)))
}

##### EXEMPLO #####

# A duração da vida de uma peça de equipamento é tal que o 
# desvio padrão populacional é igual a 5 horas. Foram amostradas 
# aleatoriamente 100 dessas peças, obtendo-se média de 500 horas. 
# Construir um intervalo de confiança para a verdadeira duração média 
# da peça com um nível de 95% de confiança.

dp <- 5
n <- 100
avg <- 500
ci <- 0.95

# CURVA NORMAL

pnormGC(bound=qnorm(ci + ((1 - ci)/2), mean = 500, sd =5), region="below", mean = 500, sd = 5, graph=TRUE)

# INTERVALO DE CONFIANÇA

interval(avg, ci, dp, n)
