##################################################
############### TESTES DE HIPÓTESE ###############
##################################################


####### FUNÇÃO PARA TESTES #######

ht <- function(x_s, x_p, sd, sd_is_pop = TRUE, n, h0, alpha) {
  # x_s: média amostral
  # x_p: média populacional
  # sd: desvio padrão
  # sd_is_pop: desvio padrão populacional fornecido é da população? (false --> d.p. da amostra)
  # n: tamanho da amostra
  # h0: contrução da hipótese nula
  # alpha: nível de significância
  
  
  # NORMALIZA VALOR
  x_obs <- (x_s - x_p)/(sd/sqrt(n))

  # OBTÉM QUANTIL
  if (h0 == "<=" | h0 == ">=" ) {
    a <- alpha
  } else if (h0 == "=") {
    a <- alpha/2
  }
  l <- 1 - a
  
  # OBTÉM VALOR CRÍTICO
  if (sd_is_pop == TRUE) {
    x_crit <- qnorm(l)
  } else if (sd_is_pop == FALSE) { 
    x_crit <- qt(l, df = n - 1)}
  
  
  # TESTA HIPÓTESE
  
  # 1. média amostral é menor ou igual à media populacional
  if (h0 == "<=") {
    print(paste("Valor observado:", x_obs))
    print(paste("Valor crítico:", x_crit))
    if (x_obs < -x_crit) {
      return("Aceita H0")
    } else {return("Rejeita H0")}
    
  # 2. média amostral é maior ou igual à media populacional    
  } else if (h0 == ">=") {
    print(paste("Valor observado:", x_obs))
    print(paste("Valor crítico:", -x_crit))
    if (x_obs > -x_crit) {
      return("Aceita H0")
    } else {return("Rejeita H0")}
    
  # 3. média amostral é igual à media populacional  
  } else if (h0 == "=") {
    print(paste("Valor observado:", abs(x_obs)))
    print(paste("Valor crítico:", x_crit))
    if (abs(x_obs) < x_crit) {
      return("Aceita H0")
    } else {return("Rejeita H0")}
    
  }}



######### EXERCÍCIOS ############

#### 1) Funcionários de uma grande firma de contabilidade
#### afirmam que a média dos salários dos contadores é menor
#### que a de seu concorrente, que é R$ 45.000. Uma amostra
#### aleatória de 30 contadores da firma mostrou que a média
#### dos salários é de R$ 43.500. Sabe-se de estudos anteiores
#### que o desvio dos salários é R$ 5.200. Teste a afirmação
#### dos funcionários ao nível de significância de 5%.

ht(43500, 45000, 5200, sd_is_pop = TRUE, n = 30, h0 = ">=", alpha = 0.05)

#### 2) Um fabricante afirma que seus cigarros contêm não mais
#### do que 30mg de nicotina. Uma amostra de 25 cigarros
#### fornece média de 31,5mg e desvio de 3mg. Ao nível de 
#### significância de 5%, os dados comprovam a afirmação
#### do fabricante?

ht(31.5, 30, 3, sd_is_pop = FALSE, n = 25, h0 = "<=", alpha = 0.05)


#### 3) Uma máquina automática de café enche pacotes segundo uma
#### distribuição normal com média μ = 500g e σ2 = 400g2. Periodicamente,
#### recolhe-se uma amostra de 16 pacotes para verificar se a produção está
#### sob controle. Suponha que a amostra apresentou X = 492g. Assumindo o nível 
#### de significância de 5%, a máquina precisa ser regulada

ht(492, 500, sqrt(400), sd_is_pop = TRUE, n = 16, h0 = "=", alpha = 0.05)


#### 4) Os registros dos últimos anos de um colégio atestam para os
#### calouros admitidos uma nota média 115 (teste vocacional). Para testar 
#### a hipótese de que a média de uma nova turma é a mesma das anteriores,
#### retirou-se uma amostra de 20 notas, obtendo-se média 118 e desvio 
#### padrão 20. Admita um nível de significância de 5% para efetuar o test

ht(118, 115, 20, sd_is_pop = FALSE, n = 20, h0 = "=", alpha = 0.05)





