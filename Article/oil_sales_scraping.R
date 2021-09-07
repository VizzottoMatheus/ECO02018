library(dplyr)

######  FONTE  #####

# https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/vendas-de-derivados-de-petroleo-e-biocombustiveis

####################




##############   POR MUNICÍPIO


# 2000 - 2018
path <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/oleo-diesel/vendas-oleodiesel-municipio-"
diesel_2000_2018 <- NULL
for (ano in 2000:2018) {
  url <- paste0(path, ano, ".csv")
  print(url)
  df <- read.csv(url, sep = ";", fileEncoding = "latin1", dec = ",")
  colnames(df) <- c("ANO", "ESTADO", "CODIBGE", "MUNICIPIO", "VENDAS")
  print(head(df))
  diesel_2000_2018 <- bind_rows(diesel_2000_2018, df)
}
diesel_2000_2018$VENDAS <- as.numeric(diesel_2000_2018$VENDAS)


# 2019
path <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/oleo-diesel/vendas-oleo-diesel-municipio-2019.csv"
diesel_2019 <- read.csv(path, sep = ";", col.names = c("ANO", "ESTADO", "CODIBGE", "MUNICIPIO", "VENDAS"))
diesel_2019$VENDAS <-  as.numeric(gsub("\\.", "", diesel_2019$VENDAS))

# CONCATENANDO

diesel_municipios <- bind_rows(diesel_2000_2018, diesel_2019)

# MUNICÍPIOS DO RIO GRANDE DO SUL
unique(diesel_municipios$ESTADO)

diesel_mun_rs <- filter(diesel_municipios, ESTADO == "Rio Grande do Sul")

vendas_anuais <- diesel_mun_rs %>%
                  group_by(ANO) %>%
                  summarise(M3 = sum(VENDAS, na.rm = TRUE))



##############   POR ESTADO

path <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vendas-derivados-petroleo-e-etanol/vendas-derivados-petroleo-etanol-m3-1990-2021.csv"
derivados <- read.csv(path, sep = ";", fileEncoding = "latin1", dec = ",")
diesel_estados <- filter(derivados, PRODUTO == "ÓLEO DIESEL")
diesel_rs <- filter(diesel_estados, UNIDADE.DA.FEDERAÇÃO == "RIO GRANDE DO SUL")

diesel_rs$MÊS <- diesel_rs$MÊS %>%
      gsub('JAN', "01", .) %>%
      gsub('FEV', "02", .) %>%
      gsub('MAR', "03", .) %>%
      gsub('ABR', "04", .) %>%
      gsub('MAI', "05", .) %>%
      gsub('JUN', "06", .) %>%
      gsub('JUL', "07", .) %>%
      gsub('AGO', "08", .) %>%
      gsub('SET', "09", .) %>%
      gsub('OUT', "10", .) %>%
      gsub('NOV', "11", .) %>%
      gsub('DEZ', "12", .)

diesel_rs$ANO_MES <- as.Date(paste0(diesel_rs$ANO, "-", diesel_rs$MÊS, "-01"))
diesel_rs <- arrange(diesel_rs, ANO_MES)

df_diesel_rs <- select(diesel_rs, c("ANO_MES", "VENDAS"))