#TAREFA 1#

pop_estimada_2015 = read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv",
                    sep = ";")

pop_2010_sexo = read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv",
                    sep = ";")

pop_2010_fx_uf = read.csv("população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv",
                       sep = ";")

pop_2010_fx_mun = read.csv("população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv",
                        sep = ";")

#Tarefa 2#

POPRE_T <- pop_estimada_2015[, c("CODMUNRES", "POPRE_T")]

POPRC <- pop_2010_sexo[, c("CODMUNRES",
                           "POPRC_T",
                           "POPRC_M",
                           "POPRC_F")]

# 15 anos ou mais
POPRC_15 <- aggregate(
  POP ~ CODMUNRES,
  data = subset(pop_2010_fx_mun,
                F_IDADE >= 15),
  FUN = sum
)

names(POPRC_15)[2] <- "POPRC_15"


# 15 a 49 anos
POPRC_15_49 <- aggregate(
  POP ~ CODMUNRES,
  data = subset(pop_2010_fx_mun,
                F_IDADE >= 15 &
                  F_IDADE <= 49),
  FUN = sum
)

names(POPRC_15_49)[2] <- "POPRC_15_49"


# 50 anos ou mais
POPRC_50 <- aggregate(
  POP ~ CODMUNRES,
  data = subset(pop_2010_fx_mun,
                F_IDADE >= 50),
  FUN = sum
)

names(POPRC_50)[2] <- "POPRC_50"


# Feminino 15+
POPRC_F_15 <- aggregate(
  POPF ~ CODMUNRES,
  data = subset(pop_2010_fx_mun,
                F_IDADE >= 15),
  FUN = sum
)

names(POPRC_F_15)[2] <- "POPRC_F_15"


# Feminino 15-49
POPRC_F_15_49 <- aggregate(
  POPF ~ CODMUNRES,
  data = subset(pop_2010_fx_mun,
                F_IDADE >= 15 &
                  F_IDADE <= 49),
  FUN = sum
)

names(POPRC_F_15_49)[2] <- "POPRC_F_15_49"

# Feminino 50+
POPRC_F_50 <- aggregate(
  POPF ~ CODMUNRES,
  data = subset(pop_2010_fx_mun,
                F_IDADE >= 50),
  FUN = sum
)

names(POPRC_F_50)[2] <- "POPRC_F_50"

# Lista de tabelas#

lista_sidra <- list(
  POPRE_T,
  POPRC,
  POPRC_15,
  POPRC_15_49,
  POPRC_50,
  POPRC_F_15,
  POPRC_F_15_49,
  POPRC_F_50
)

SIDRA_SC <- Reduce(function(x,y)
  merge(x,y, by = "CODMUNRES", all = TRUE),
  lista_sidra)

SIDRA_SC$ANO <- 2015

SIDRA_SC$NIVEL <- ifelse(nchar(SIDRA_SC$CODMUNRES) == 2,
                         "UF",
                         "MUNICIPIO")
SIDRA_SC <- SIDRA_SC[, c(
  "ANO",
  "NIVEL",
  "CODMUNRES",
  "POPRE_T",
  "POPRC_T",
  "POPRC_M",
  "POPRC_F",
  "POPRC_15",
  "POPRC_15_49",
  "POPRC_50",
  "POPRC_F_15",
  "POPRC_F_15_49",
  "POPRC_F_50"
)]
