#ETAPA 2#

#Tarefa 1#

dados_sim = read.csv2("Mortalidade_Geral_2015.csv", sep = ";", header = TRUE)

dim(dados_sim)
str(dados_sim)
head(dados_sim)
summary(dados_sim)

#Tarefa 2#

dados_sim_1 = dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17,
                             35, 36, 37, 47, 77, 84)]

names(dados_sim_1) = c("CONTADOR", "TIPOBITO", "DTOBITO", "DTNASC",
                        "IDADE", "SEXO", "RACACOR", "ESC2010",
                        "CODMUNRES", "TPMORTEOCO", "OBITOGRAV",
                        "OBITOPUERP", "CAUSABAS", "TPOBITOCOR",
                        "MORTEPARTO")

str(dados_sim_1)
dim(dados_sim_1)

#Tarefa 3#

dados_sim_1$CODMUNRES = as.character(dados_sim_1$CODMUNRES)

dados_sim_2 = dados_sim_1[
  substr(dados_sim_1$CODMUNRES, 1, 2) == "42",
]

dim(dados_sim_2)
str(dados_sim_2)
write.csv(dados_sim_2, "dados_sim_2.csv", row.names = FALSE)

table(substr(dados_sim_2$CODMUNRES, 1, 2))

#Tarefa 4#

variaveis = c("TIPOBITO", "SEXO", "RACACOR",
               "TPMORTEOCO", "OBITOGRAV",
               "OBITOPUERP", "CAUSABAS",
               "TPOBITOCOR", "MORTEPARTO")

for(v in variaveis){
  
  cat("\n========================\n")
  cat("Variável:", v, "\n")
  cat("========================\n")
  
  print(table(dados_sim_2[[v]], useNA = "ifany"))
}

#Tarefa 5#

dados_sim_2$TIPOBITO[dados_sim_2$TIPOBITO == 9] = NA

dados_sim_2$SEXO[dados_sim_2$SEXO == 0] = NA

dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] = NA

dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] = NA

dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] = NA

dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] = NA

dados_sim_2$TPOBITOCOR[dados_sim_2$TPOBITOCOR == 9] = NA

dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] = NA


dados_sim_2$IDADE[dados_sim_2$IDADE == 99] = NA

#Tarefa 6#

dados_sim_2$TIPOBITO <- factor(dados_sim_2$TIPOBITO,
                               levels = c(1,2),
                               labels = c("Fetal", "Não fetal")
)

dados_sim_2$SEXO <- factor(dados_sim_2$SEXO,
                           levels = c(1,2),
                           labels = c("Masculino", "Feminino")
)

dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR,
                              levels = c(1,2,3,4,5),
                              labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
)

dados_sim_2$TPMORTEOCO <- factor(dados_sim_2$TPMORTEOCO,
                                 levels = c(1,2,3),
                                 labels = c("Hospital", "Outro estabelecimento de saúde", "Domicílio")
)

dados_sim_2$OBITOGRAV <- factor(dados_sim_2$OBITOGRAV,
                                levels = c(1,2,3),
                                labels = c("Sim", "Não", "Ignorado")
)

dados_sim_2$OBITOPUERP <- factor(dados_sim_2$OBITOPUERP,
                                 levels = c(1,2,3),
                                 labels = c("Sim", "Não", "Ignorado")
)

dados_sim_2$TPOBITOCOR <- factor(dados_sim_2$TPOBITOCOR,
                                 levels = c(1,2),
                                 labels = c("Materno", "Não materno")
)

dados_sim_2$MORTEPARTO <- factor(dados_sim_2$MORTEPARTO,
                                 levels = c(1,2),
                                 labels = c("Sim", "Não")
)

str(dados_sim_2)

#Tarefa 7#

dados_sim_2$MUNIC = dados_sim_2$CODMUNRES

TO = aggregate(CONTADOR ~ MUNIC, dados_sim_2, length)
names(TO)[2] = "TO"

TORCR = aggregate(CONTADOR ~ MUNIC, dados_sim_2,
                   function(x) sum(complete.cases(dados_sim_2)))
names(TORCR)[2] = "TORCR"

TO_NN = aggregate(CAUSABAS ~ MUNIC, dados_sim_2,
                   function(x) sum(substr(x,1,1) %in% c("V","W","X","Y"),
                                   na.rm=TRUE))
names(TO_NN)[2] = "TO_NN"

TO_N = aggregate(CAUSABAS ~ MUNIC, dados_sim_2,
                  function(x) sum(!substr(x,1,1) %in% c("V","W","X","Y"),
                                  na.rm=TRUE))
names(TO_N)[2] = "TO_N"

TO_CB_I = aggregate(CAUSABAS ~ MUNIC, dados_sim_2,
                     function(x) sum(substr(x,1,1) %in% c("A","B"),
                                     na.rm=TRUE))
names(TO_CB_I)[2] = "TO_CB_I"

TO_CB_N = aggregate(CAUSABAS ~ MUNIC, dados_sim_2,
                     function(x) sum(substr(x,1,1) %in% c("C","D"),
                                     na.rm=TRUE))
names(TO_CB_N)[2] = "TO_CB_N"

TO_CB_C = aggregate(CAUSABAS ~ MUNIC, dados_sim_2,
                     function(x) sum(substr(x,1,1)=="I",
                                     na.rm=TRUE))
names(TO_CB_C)[2] = "TO_CB_C"

TO_CB_R = aggregate(CAUSABAS ~ MUNIC, dados_sim_2,
                     function(x) sum(substr(x,1,1)=="J",
                                     na.rm=TRUE))
names(TO_CB_R)[2] = "TO_CB_R"

TO_M = aggregate(SEXO ~ MUNIC, dados_sim_2,
                  function(x) sum(x=="Masculino", na.rm=TRUE))
names(TO_M)[2] = "TO_M"

TO_F = aggregate(SEXO ~ MUNIC, dados_sim_2,
                  function(x) sum(x=="Feminino", na.rm=TRUE))
names(TO_F)[2] = "TO_F"

TO_F_IF = aggregate(IDADE ~ MUNIC, dados_sim_2,
                     function(x) sum(x >=15 & x <=49, na.rm=TRUE))
names(TO_F_IF)[2] = "TO_F_IF"


TO_FT = aggregate(TIPOBITO ~ MUNIC, dados_sim_2,
                   function(x) sum(x=="Fetal", na.rm=TRUE))
names(TO_FT)[2] = "TO_FT"

TO_NT = aggregate(IDADE ~ MUNIC, dados_sim_2,
                   function(x) sum(x >=0 & x <=27, na.rm=TRUE))
names(TO_NT)[2] = "TO_NT"

TO_NT_P = aggregate(IDADE ~ MUNIC, dados_sim_2,
                     function(x) sum(x >=0 & x <=6, na.rm=TRUE))
names(TO_NT_P)[2] = "TO_NT_P"

TO_NT_T = aggregate(IDADE ~ MUNIC, dados_sim_2,
                     function(x) sum(x >=7 & x <=27, na.rm=TRUE))
names(TO_NT_T)[2] = "TO_NT_T"

TO_PNT = aggregate(IDADE ~ MUNIC, dados_sim_2,
                    function(x) sum(x >=28 & x <=364, na.rm=TRUE))
names(TO_PNT)[2] = "TO_PNT"

lista_variaveis = list(
  TO,
  TORCR,
  TO_NN,
  TO_N,
  TO_CB_I,
  TO_CB_N,
  TO_CB_C,
  TO_CB_R,
  TO_M,
  TO_F,
  TO_F_IF,
  TO_FT,
  TO_NT,
  TO_NT_P,
  TO_NT_T,
  TO_PNT
)

base_final = Reduce(function(x,y)
  merge(x,y, by="MUNIC", all=TRUE),
  lista_variaveis)

uf = base_final[1,]
uf[1,] = NA

uf$MUNIC = "42"

uf$TO = nrow(dados_sim_2)

SIM_SC = rbind(uf, base_final)

SIM_SC$ANO = 2015

SIM_SC$NIVEL = ifelse(SIM_SC$MUNIC=="42",
                       "UF",
                       "MUNICIPIO")

SIM_SC$CODMUNRES = SIM_SC$MUNIC

SIM_SC = SIM_SC[, c("ANO","NIVEL","CODMUNRES",
                     names(base_final)[-1])]

write.csv(SIM_SC, "SIM_SC.csv", row.names = FALSE)
