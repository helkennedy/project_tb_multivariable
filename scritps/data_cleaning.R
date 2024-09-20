library(dplyr)

data_cleaning <- function(data){
  # Define códigos e siglas dos estados
  codigos_uf <- c("12", "27", "13", "16", "29", "23", "53", "32", "52", "21", "51", "50", "31", "15", "25", "41", "26", "22", "33", "24", "43", "11", "14", "42", "35", "28", "17")
  siglas_uf <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
  
  # Define códigos e categorias para forma de tuberculose
  cod_forma = c(1, 2, 3)
  forma = c("Pulmonar", "Extrapulmonar", "Pulmonar+Extrapul")
  
  # Define códigos e categorias para evolução
  cod_ep = c(1,2,3,4,5,6,7,8,9,10, NA, 0, 99)
  ep = c("Pleural", "Gang. Perif.", "Geniturinária", "Óssea", "Ocular", "Miliar", 
         "Meningoencefálico", "Cutânea", "Laringea", "Outra", "Pulmonar", NA, NA)
  
  # Define códigos e categorias para comorbidades
  cod_comorbidades = c(2, 1, 9)
  comorbidades = c("Não", "Sim", NA)
  
  # Define códigos e categorias para encerramento
  cod_encerramento <- c(1,2,3,4,5,6,7,8,9,10)
  encerramento <- c(
    "Cura", "Abandono", "Óbito por TB", "Óbito por outras causas",
    "Transferência", "Mudança de Diagnóstico", "TB-DR", "Mudança de Esquema")
  
  # Manipulação dos dados
  data <- data |>
    mutate(
      ANO_NASC = as.integer(ANO_NASC),  # Converte o ano de nascimento para inteiro.
      SG_UF_NOT = siglas_uf[match(SG_UF_NOT, codigos_uf)],  # Substitui códigos de UF por siglas.
      FORMA = forma[match(FORMA, cod_forma)],  # Substitui códigos de forma por categorias.
      
      EXTRAPU1_N = ep[match(EXTRAPU1_N, cod_ep)],  # Substitui códigos de evolução por categorias.
      
      SITUA_ENCE = encerramento[match(SITUA_ENCE , cod_encerramento)],  # Substitui códigos de encerramento por categorias.
      
      # Manipulação das comorbidades
      AGRAVAIDS = comorbidades[match(AGRAVAIDS , cod_comorbidades)],
      AGRAVAIDS = fct_relevel(AGRAVAIDS, "Não", "Sim"),  # Reordena fatores para referência.
      
      AGRAVALCOO = comorbidades[match(AGRAVALCOO, cod_comorbidades)],
      AGRAVALCOO = fct_relevel(AGRAVALCOO, "Não", "Sim"),
      
      AGRAVDIABE = comorbidades[match(AGRAVDIABE, cod_comorbidades)],
      AGRAVDIABE = fct_relevel(AGRAVDIABE, "Não", "Sim"),
      
      AGRAVDROGA = comorbidades[match(AGRAVDROGA, cod_comorbidades)],
      AGRAVDROGA = fct_relevel(AGRAVDROGA, "Não", "Sim"),
      
      AGRAVDOENC = comorbidades[match(AGRAVDOENC, cod_comorbidades)],
      AGRAVDOENC = fct_relevel(AGRAVDOENC, "Não", "Sim"),
      
      AGRAVTABAC = comorbidades[match(AGRAVTABAC, cod_comorbidades)],
      AGRAVTABAC = fct_relevel(AGRAVTABAC, "Não", "Sim"),
      
      POP_LIBER = comorbidades[match(POP_LIBER, cod_comorbidades)],
      POP_LIBER = fct_relevel(POP_LIBER, "Não", "Sim"),  # Reordena para referência.
      
      POP_SAUDE = comorbidades[match(POP_SAUDE, cod_comorbidades)],
      POP_SAUDE = fct_relevel(POP_SAUDE, "Não", "Sim"),
      
      POP_RUA = comorbidades[match(POP_RUA, cod_comorbidades)],
      POP_RUA = fct_relevel(POP_RUA, "Não", "Sim"),
      
      POP_IMIG = comorbidades[match(POP_IMIG, cod_comorbidades)],
      POP_IMIG = fct_relevel(POP_IMIG, "Não", "Sim"),
      
      SG_UF_AT = siglas_uf[match(SG_UF_AT, codigos_uf)],  # Substitui códigos de UF de atendimento por siglas.
      SG_UF_2 = siglas_uf[match(SG_UF_2, codigos_uf)]  # Substitui códigos de UF secundária por siglas.
    )
  
  return(data)  # Retorna o data frame limpo.
}
