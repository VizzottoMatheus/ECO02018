library(ggplot2)


# DATA
Y <- c(75,  85,  98, 108, 118, 135, 145, 157, 175, 180)
X <- c(80, 100, 120, 140, 160, 180, 200, 220, 240, 260)
df <- as.data.frame(cbind(Y, X))
colnames(df) <- c("CONSUMO", "RENDA")
df

# a) Faça a análise descritiva dos dados e interprete o teste Jarque-Bera (no Eviews).
# b) Estime a função consumo (Y=consumo; X=renda).
# c) Interprete os parâmetros da equação obtida.
# d) Faça a inferência estatística, ou seja, interprete os resultados para os testes "t" , R2 e F.
# e) Mostre o gráfico dos valores de Y (observados, estimados) e resíduos e interprete o mesmo.

# a)

### UNIDIMENSIONAL

# CONSUMO
mean(df$CONSUMO) # 127,6
median(df$CONSUMO) # 50% DAS OBSERVAÇÕES TÊM ATÉ 126,5 DE CONSUMO
ggplot(df, aes(y = CONSUMO)) + geom_boxplot() + theme_bw()

# RENDA
mean(df$RENDA) # 170
median(df$RENDA) # 50% DAS OBSERVAÇÕES TÊM ATÉ 170 DE CONSUMO
# ATENÇÃO: MÉDIA = MEDIANA
ggplot(df, aes(y = RENDA)) + geom_boxplot() + theme_bw()

### BIDIMENSIONAL
cor(df$CONSUMO, df$RENDA) # HÁ CORRELAÇÃO ELEVADA ENTRE OS DADOS DE CONSUMO E RENDA
ggplot(df, aes(x = RENDA, y = CONSUMO)) + 
  geom_point() + 
  theme_bw() + 
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0, 300))

# b)

model <- lm(df$CONSUMO ~ df$RENDA)

summary(model) 

# c)

# a regressão entre os dados disponíveis estimou que,
# a cada unidade a mais de renda do indivíduo, seu consumo
# tende a aumentar 0,605 unidades. O intercepto apresentou
# um valor de 24,6, o que faz referência ao consumo autônomo:
# mesmo sem renda disponível, o indivíduo ainda apresentará
# algum nível de consumo.

# d)

# O R2 é a medida que mostra quão bem uma variável explica
# outra (R2 = 1 - (Soma do quadrado dos resíduos)/(Soma dos quadrados totais)).
# Essa medida no modelo em questão mostrou que 99,6% da variação no consumo
# é explicada pela variação na renda.

# o teste t do modelo se mostrou significativo ao nível de 1%
# tanto para o intercepto quanto para o coeficiente de renda, 
# o que nos leva a rejeitar a hipótese nula de que não existe 
# relação entre as variáveis.

# O teste F também se mostrou significativo ao nível de 1%,
# indicando que a inclusão da variável independente no modelo
# proporciona melhor explicação do consumo do que utilizando
# somente a constante B0.

# e)
ggplot(df, aes(x = RENDA, y = CONSUMO)) + 
  geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE, col = "red", alpha = 0.6) +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0, 300))

hist(model$residuals)
par(mfrow = c(2,2))
plot(model)

df_model <- as.data.frame(cbind(model$fitted.values, model$residuals, 0))
colnames(df_model) <- c("ESTIMADO", "RESIDUOS", "AXIS")

ggplot(df_model, aes(x = ESTIMADO, y = RESIDUOS)) +
  geom_point(size = 2) +
  geom_line(aes(x = ESTIMADO, y = AXIS), linetype = 2) +
  theme_bw() +
  theme(axis.line.x = element_line(color = "black")) +
  scale_y_continuous(limits = c(-6, 6)) +
  scale_x_continuous(limits = c(60, 200)) 

# Os resíduos flutuam aleatoriamente em torno da origem,
# o que sugere ser razoável assumir uma relação linear
# entre as variáveis. Além disso, não fica explícita 
# uma alteração na variância nem é possível caracterizar
# algum dos resíduos como outlier.

#summary(lm(df_model$RESIDUOS ~ df_model$ESTIMADO))
