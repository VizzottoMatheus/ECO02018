library(tigerstats)


#################### FUNÇÕES ##############

scaler <- function(x, a, d) {
  z <- (x - a)/d
  return(z)
}

################### EXEMPLOS ##############


#### 1) As vendas de um determinado produto tem distribuicao aproximadamente
#### normal, com media 500 e desvio padrao 50. Se a empresa decide fabricar
#### 600 unidades no mes em estudo, qual e a probabilidade de que nao possa
#### atender a todos os pedidos desse mes, por estar com a producao esgotada?
  

avg <- 500
dp <- 50
x <- 600

# padronizando
z <- scaler(x, avg, dp)

# calculando probabilidade
pnormGC(z, graph = TRUE, region = "above")
prob <- (1 - pnorm(z))*100

print(paste("Probabilidade de que vendas > produção:", round(prob,2), "%")) # 2,28%


#### 2) O diâmetro X de rolamentos de esfera fabricados por certa fábrica tem
#### distribuição N(0,6140; 0,0025). O lucro T de cada esfera depende do seu diâmetro, e
#### T = 0,10 se a esfera é boa (0,6100 < X < 0,618)
#### T = 0,05 se a esfera é recuperável (0,6080 < X < 0,6100) ou (0,6180 < X < 0,6200)
#### T = -0,10 se a esfera é defeituosa (X < 0,6080 ou X > 0,62)
#### Calcular as probabilidades de as esferas serem boas, recuperáveis e defeituosas

avg <- 0.614
dp <- 0.0025

# BOAS

X1 <- 0.61
X2 <- 0.618

Z1 <-scaler(X1, avg, dp)
Z2 <-scaler(X2, avg, dp)

pnormGC(bound = c(Z1, Z2), region = "between", graph = TRUE)

prob_boas <- pnorm(Z2) - pnorm(Z1)
print(round(prob_boas*100,2)) # 89,04%

# RECUPERÁVEIS

X1 <- 0.6080
X2 <- 0.61
Z1 <-scaler(X1, avg, dp)
Z2 <-scaler(X2, avg, dp)
pnormGC(bound = c(Z1, Z2), region = "between", graph = TRUE)
prob_rec1 <- pnorm(Z2) - pnorm(Z1)

X3 <- 0.6180
X4 <- 0.62
Z3 <-scaler(X3, avg, dp)
Z4 <-scaler(X4, avg, dp)
pnormGC(bound = c(Z3, Z4), region = "between", graph = TRUE)
prob_rec2 <- pnorm(Z4) - pnorm(Z3)

print(round((prob_rec1 + prob_rec2)*100,2)) # 9,32%

# DEFEITUOSAS

X1 <- 0.608
X2 <- 0.62

Z1 <-scaler(X1, avg, dp)
pnormGC(Z1, graph = TRUE, region = "below")
prob_def1 <- pnorm(Z1)

Z2 <-scaler(X2, avg, dp)
pnormGC(Z2, graph = TRUE, region = "above")
prob_def2 <- 1 - pnorm(Z2)

print(round((prob_def1 + prob_def2)*100,2)) # 1,64%
