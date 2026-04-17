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

table(dados_sinasc_2$LOCNASC)

table(dados_sinasc_2$ESTCIVMAE)

table(dados_sinasc_2$GESTACAO)

table(dados_sinasc_2$GRAVIDEZ)

table(dados_sinasc_2$PARTO)

table(dados_sinasc_2$SEXO)

table(dados_sinasc_2$APGAR5)

table(dados_sinasc_2$RACACOR)

table(dados_sinasc_2$IDANOMAL)

table(dados_sinasc_2$ESCMAE2010)

table(dados_sinasc_2$RACACORMAE)

table(dados_sinasc_2$TPAPRESENT)

table(dados_sinasc_2$TPROBSON)

table(dados_sinasc_2$PARIDADE)

table(dados_sinasc_2$KOTELCHUCK)

table(dados_sinasc_2$LOCNASC, useNA = "ifany")

variaveis <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO",
               "SEXO", "APGAR5", "RACACOR", "IDANOMAL", "ESCMAE2010",
               "RACACORMAE", "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK")

for (v in variaveis) {
  cat("\nFrequência de", v, ":\n")
  print(table(dados_sinasc_2[[v]], useNA = "ifany"))
}

vars_cat <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO",
              "SEXO", "RACACOR", "IDANOMAL", "ESCMAE2010",
              "RACACORMAE", "TPAPRESENT", "PARIDADE", "KOTELCHUCK")

for (v in vars_cat) {
  dados_sinasc_2[[v]][dados_sinasc_2[[v]] == 9] <- NA
}

dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] <- NA

dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] <- NA

dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] <- NA

dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] <- NA

dados_sinasc_2$SEMAGESTAC[dados_sinasc_2$SEMAGESTAC == 99] <- NA

summary(dados_sinasc_2)

dados_sinasc_2$KOTELCHUCK <- factor(dados_sinasc_2$KOTELCHUCK,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado")
)

dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO,
                              levels = c(1,2),
                              labels = c("Masculino", "Feminino")
)

dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO,
                               levels = c(1,2),
                               labels = c("Vaginal", "Cesáreo")
)

dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ,
                                  levels = c(1,2,3),
                                  labels = c("Única", "Dupla", "Tripla ou mais")
)

dados_sinasc_2$GESTACAO <- factor(dados_sinasc_2$GESTACAO,
                                  levels = c(1,2,3,4,5,6),
                                  labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas",
                                             "32 a 36 semanas", "37 a 41 semanas", "42 semanas ou mais")
)

dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE,
                                   levels = c(1,2,3,4,5),
                                   labels = c("Solteira", "Casada", "Viúva", "Separada", "União consensual")
)

dados_sinasc_2$LOCNASC <- factor(dados_sinasc_2$LOCNASC,
                                 levels = c(1,2,3,4),
                                 labels = c("Hospital", "Outro estabelecimento de saúde", "Domicílio", "Outros")
)

dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR,
                                 levels = c(1,2,3,4,5),
                                 labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
)

dados_sinasc_2$RACACORMAE <- factor(dados_sinasc_2$RACACORMAE,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
)

dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL,
                                  levels = c(1,2),
                                  labels = c("Sim", "Não")
)

dados_sinasc_2$TPAPRESENT <- factor(dados_sinasc_2$TPAPRESENT,
                                    levels = c(1,2,3),
                                    labels = c("Cefálica", "Pélvica", "Transversa")
)

dados_sinasc_2$PARIDADE <- factor(dados_sinasc_2$PARIDADE,
                                  levels = c(1,2,3),
                                  labels = c("Primípara", "Secundípara", "Multípara")
)

str(dados_sinasc_2)


dados_sinasc_2$F_PESO <- ifelse(is.na(dados_sinasc_2$PESO), NA,
                                ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                                       ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                              "Macrossomia")))

dados_sinasc_2$F_PESO <- factor(dados_sinasc_2$F_PESO,
                                levels = c("Baixo peso", "Peso normal", "Macrossomia"))

dados_sinasc_2$F_IDADE <- ifelse(is.na(dados_sinasc_2$IDADEMAE), NA,
                                 ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                        ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                               ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                                      ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                             ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                                    ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                           ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                                  ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                         "50+")))))))))

dados_sinasc_2$F_IDADE <- factor(dados_sinasc_2$F_IDADE,
                                 levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"))

dados_sinasc_2$F_APGAR5 <- ifelse(is.na(dados_sinasc_2$APGAR5), NA,
                                  ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal"))

dados_sinasc_2$F_APGAR5 <- factor(dados_sinasc_2$F_APGAR5,
                                  levels = c("Baixo", "Normal"))
dados_sinasc_2$PERIG <- ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES,
                               "Não", "Sim")

dados_sinasc_2$PERIG <- factor(dados_sinasc_2$PERIG,
                               levels = c("Não", "Sim"))
dados_sinasc_2$ESTCIV <- ifelse(is.na(dados_sinasc_2$ESTCIVMAE), NA,
                                ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira","Viúva","Separada"),
                                       "Sem companheiro",
                                       ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada","União consensual"),
                                              "Com companheiro", NA)))

dados_sinasc_2$ESTCIV <- factor(dados_sinasc_2$ESTCIV,
                                levels = c("Sem companheiro", "Com companheiro"))

str(dados_sinasc_2)

table(dados_sinasc_2$F_PESO, useNA = "ifany")
table(dados_sinasc_2$F_IDADE, useNA = "ifany")
table(dados_sinasc_2$F_APGAR5, useNA = "ifany")
table(dados_sinasc_2$PERIG)
table(dados_sinasc_2$ESTCIV, useNA = "ifany")
