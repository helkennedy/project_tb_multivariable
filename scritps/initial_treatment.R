library(read.dbc)
library(dplyr)

#------- 1. OBTENDO O NUMERADOR | CASOS----

# 1.1 | Download de dados do servidor do DATASUS


"https://portalsinan.saude.gov.br/images/documentos/Agravos/Tuberculose/DICI_DADOS_NET_Tuberculose_23_07_2020.pdf"
"https://nepas.ufsc.br/files/2012/09/DICION%C3%81RIO-DE-DADOS-%E2%80%93-SINAN-NET-%E2%80%93-VERS%C3%83O-4-.-0.pdf"


{TUBEBR14 <- read.dbc::read.dbc("TUBEBR/TUBEBR14.dbc", as.is=T)
TUBEBR15 <- read.dbc::read.dbc("TUBEBR/TUBEBR15.dbc", as.is=T)
TUBEBR16 <- read.dbc::read.dbc("TUBEBR/TUBEBR16.dbc", as.is=T)
TUBEBR17 <- read.dbc::read.dbc("TUBEBR/TUBEBR17.dbc", as.is=T)
TUBEBR18 <- read.dbc::read.dbc("TUBEBR/TUBEBR18.dbc", as.is=T)
TUBEBR19 <- read.dbc::read.dbc("TUBEBR/TUBEBR19.dbc", as.is=T)
TUBEBR20 <- read.dbc::read.dbc("TUBEBR/TUBEBR20.dbc", as.is=T)
TUBEBR21 <- read.dbc::read.dbc("TUBEBR/TUBEBR21.dbc", as.is=T)
TUBEBR22 <- read.dbc::read.dbc("TUBEBR/TUBEBR22.dbc", as.is=T)
TUBEBR23 <- read.dbc::read.dbc("TUBEBR/TUBEBR23.dbc", as.is=T)
TUBEBR <- rbind(TUBEBR14, TUBEBR15, TUBEBR16, TUBEBR17, TUBEBR18, TUBEBR19,
                TUBEBR20, TUBEBR21, TUBEBR22, TUBEBR23)

rm(TUBEBR14, TUBEBR15, TUBEBR16, TUBEBR17, TUBEBR18, TUBEBR19, TUBEBR20,
   TUBEBR21, TUBEBR22, TUBEBR23)}

#----Tratamento de dados 1-----

colunas = c("NU_ANO","SG_UF_NOT", "ANO_NASC", "CS_SEXO", "POP_LIBER",
            "POP_RUA", "POP_SAUDE", 'POP_IMIG', 'FORMA', "EXTRAPU1_N", "EXTRAPU2_N",
            'AGRAVAIDS', 'AGRAVALCOO', 'AGRAVDIABE', 'AGRAVDOENC', 'AGRAVDROGA',
            "AGRAVTABAC", 'HIV', 'SG_UF_AT', 'SG_UF_2', 'TRATSUP_AT', 'SITUA_ENCE')

integer = c("NU_ANO", "ANO_NASC", "POP_LIBER", "POP_RUA", "POP_SAUDE",
            'POP_IMIG', 'FORMA', "EXTRAPU1_N", "EXTRAPU2_N", 'AGRAVAIDS', 'AGRAVALCOO',
            'AGRAVDIABE', 'AGRAVDOENC', "AGRAVDROGA", "AGRAVTABAC", 'HIV', 'TRATSUP_AT',
            'SITUA_ENCE')

names(TUBEBR)

tb_filtrado = TUBEBR[, (names(TUBEBR) %in% colunas)]

str(tb_filtrado)

tb_filtrado <- tb_filtrado %>% mutate_at(vars(integer), as.integer)

str(tb_filtrado)

tb_filtrado <- dplyr::filter(tb_filtrado, NU_ANO < 2024 & NU_ANO > 2013)

unique(tb_filtrado$FORMA)
unique(tb_filtrado$EXTRAPU1_N)

colunas_para_converter = c('POP_RUA', 'POP_SAUDE', 'POP_IMIG', 'AGRAVAIDS',
                           'AGRAVALCOO', 'AGRAVDIABE', 'AGRAVDOENC', 'AGRAVDROGA', 'AGRAVTABAC')

tb_filtrado <- tb_filtrado %>% mutate_at(
  vars(colunas_para_converter),~ifelse(. == 3, NA, .))

write.csv(tb_filtrado, "TUBEBR/tb_filtrado.csv", row.names=FALSE)

rm(TUBEBR, colunas, temp)

rm(colunas, colunas_para_converter, integer)
#----Obtenção de dados 1-----