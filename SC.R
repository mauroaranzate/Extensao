dados_sinasc = read.csv2("SINASC_2015.csv", sep = ";", header = TRUE)

dim(dados_sinasc)
str(dados_sinasc)
head(dados_sinasc)
summary(dados_sinasc)

dados_sinasc_1 <- dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 
                                   19, 21, 22, 23, 24, 35, 38, 
                                   44, 46, 48, 59, 60, 61)]

colnames(dados_sinasc_1) <- c("CONTADOR", "CODMUNNASC", "LOCNASC", "IDADEMAE", "ESTCIVMAE",
                              "CODMUNRES", "GESTACAO", "GRAVIDEZ", "PARTO",
                              "SEXO", "APGAR5", "RACACOR", "PESO", "IDANOMAL",
                              "ESCMAE2010", "RACACORMAE", "SEMAGESTAC", "CONSPRENAT",
                              "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK")

dim(dados_sinasc_1)
str(dados_sinasc_1)
head(dados_sinasc_1)

uf = substr(dados_sinasc_1$CODMUNRES, 1, 2)
dados_sinasc_2 <- dados_sinasc_1[uf == "42", ]

nrow(dados_sinasc_2)

dim(dados_sinasc_2)
head(dados_sinasc_2)

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

nrow(dados_sinasc_2)
