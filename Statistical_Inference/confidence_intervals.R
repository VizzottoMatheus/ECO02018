library(tigerstats)

######################################## FUNÇÕES #########################################

# AJUSTA % DE CONFIANÇA

q_adj <- function(ci) {
  x <- ci + ((1-ci)/2)
  return(x)
}

# ENCONTRA LIMITES INFERIOR E SUPERIOR

interval <- function(avg, ci, sd, n) {
  q <- q_adj(ci)    
  z <- qnorm(q)
  error <- z*(sd/sqrt(n))
  sup <- avg + error
  inf <- avg - error
  print(paste0("Limite inferior: ", round(inf,2)))
  print(paste0("Limite superior: ", round(sup,2)))
}

####################################### EXEMPLO ##########################################

# A duração da vida de uma peça de equipamento é tal que o 
# desvio padrão populacional é igual a 5 horas. Foram amostradas 
# aleatoriamente 100 dessas peças, obtendo-se média de 500 horas. 
# Construir um intervalo de confiança para a verdadeira duração média 
# da peça com um nível de 95% de confiança.

dp <- 5 
n <- 100
avg <- 500
ci <- 0.95


# PARA ENCONTRAR Z COM UM NÍVEL DE CONFIANÇA DE 95%, DEVE-SE SOMAR O 
# ALFA (1 - IC) À ESQUERDA DO INTERVALO:

q <- q_adj(ci)
print(q)  

# O INTERVALO DE CONFINAÇA DE 95% CORRESPONDE
# AO QUANTIL 97,5% DE UMA DISTRIBUIÇÃO NORMAL

pnormGC(bound=qnorm(ci + ((1 - ci)/2)), region="below", graph = TRUE)

# ENCONTRAMOS Z = 1,96. ESTE É O INTERVALO QUE PROCURAMOS:

pnormGC(bound = c(-qnorm(q), qnorm(q)), region = "between", graph = TRUE)


# INTERVALO DE CONFIANÇA

interval(avg, ci, dp, n)

# Limite inferior do intervalo: 499.02
# Limite superior do intervalo: 500.98
