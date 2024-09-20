#------- Pacotes-------
library(tidyverse)  # Carrega o pacote tidyverse, que inclui várias funções úteis para manipulação de dados.
library(dplyr)      # Carrega o pacote dplyr, que é parte do tidyverse e fornece funções para manipulação de dados.
library(hash)       # Carrega o pacote hash, que permite criar tabelas hash para armazenamento eficiente de dados.
library(broom)      # Carrega o pacote broom, que transforma resultados de modelos em data frames.

# Carrega scripts externos para limpeza de dados e formatação de p-valores.
source("scripts/data_cleaning.R")
source("scripts/format_p_values.R")

#---------- Carregamento dos dados ----
tb_filtrado = read.csv("TUBEBR/tb_filtrado.csv") |> 
  data_cleaning()  # Lê um arquivo CSV e aplica a função de limpeza de dados.

#----------- Regressão Logística----------
model_lm <- function(df, epm) {
  # Filtra para a evolução de interesse e mantém apenas casos completos das populações
  tb_form <- df %>%
    dplyr::filter(EXTRAPU1_N == ep | EXTRAPU1_N == 'Pulmonar') %>%
    mutate(EXTRAPU1_N = fct_relevel(EXTRAPU1_N, "Pulmonar", ep)) %>%
    dplyr::filter(complete.cases(POP_LIBER, POP_RUA, POP_SAUDE, POP_IMIG))  # Mantém apenas linhas completas.
  
  # Modelo de regressão logística incluindo todas as populações
  modelo <- glm(
    formula = EXTRAPU1_N ~ POP_LIBER + POP_RUA + POP_SAUDE + POP_IMIG, 
    data = tb_form, family = binomial()  # Ajusta um modelo de regressão logística.
  )
  return(modelo)  # Retorna o modelo ajustado.
}

#---- Coletar dados do modelo ----
processar_dados <- function(df, ep, modelo_fn) {
  modelo <- modelo_fn(df, ep)  # Chama a função de modelo com os dados e a evolução.
  
  df_estatistics <- tidy(modelo, conf.int = TRUE, exponentiate = TRUE)  # Organiza os resultados do modelo em um data frame.
  
  or <- df_estatistics$estimate[2:5]  # Extrai as razões de chances (ORs) das populações.
  p <- format_p_values(df_estatistics$p.value[2:5])  # Formata os p-valores.
  
  return(list(or, p))  # Retorna uma lista com ORs e p-valores.
}

# Função principal
main <- function(df, ep) {
  ep_df <- processar_dados(df, ep, model_lm)  # Processa os dados para a evolução especificada.
  return(ep_df)  # Retorna os resultados.
}

# Criar dataframe com os resultados
df_function <- function(ep_df, ep) {
  ep_df <- data.frame(
    'populacoes' = c("POP_LIBER", "POP_RUA", "POP_SAUDE", "POP_IMIG"),  # Nomes das populações.
    'OR' = ep_df[[1]],  # Razões de chances.
    'p_value' = ep_df[[2]],  # P-valores.
    'Evolucao' = rep(ep, 4)  # Repete a evolução para cada população.
  )
  return(ep_df)  # Retorna o data frame com os resultados.
}

# Análises para as evoluções
eps = c("Pleural", "Gang. Perif.", "Geniturinária", "Óssea", "Ocular", "Miliar", 
        "Meningoencefálico", "Cutânea", "Laringea", "Outra", "Pulmonar")  # Lista de evoluções a serem analisadas.

# Executa a função principal para cada evolução e armazena os resultados
am_pleural <- main(tb_filtrado, eps[1])
am_pleural_est <- df_function(am_pleural, eps[1])

# Repetição do processo para outras evoluções
am_gang.perif <- main(tb_filtrado, eps[2])
am_gang.perif_est <- df_function(am_gang.perif, eps[2])

am_genit <- main(tb_filtrado, eps[3])
am_genit_est <- df_function(am_genit, eps[3])

am_ossea <- main(tb_filtrado, eps[4])
am_ossea_est <- df_function(am_ossea, eps[4])

am_ocular <- main(tb_filtrado, eps[5])
am_ocular_est <- df_function(am_ocular, eps[5])

am_miliar <- main(tb_filtrado, eps[6])
am_miliar_est <- df_function(am_miliar, eps[6])

am_mtb <- main(tb_filtrado, eps[7])
am_mtb_est <- df_function(am_mtb, eps[7])

am_cutan <- main(tb_filtrado, eps[8])
am_cutan_est <- df_function(am_cutan, eps[8])

am_laring <- main(tb_filtrado, eps[9])
am_laring_est <- df_function(am_laring, eps[9])

#---- Exportar resultado da análise multivariada ----
data_combined <- bind_rows(
  am_cutan_est,
  am_gang.perif_est,
  am_genit_est, 
  am_laring_est, 
  am_miliar_est, 
  am_mtb_est,
  am_ocular_est, 
  am_ossea_est,
  am_pleural_est
)  # Combina todos os resultados em um único data frame.

data_combined <- data_combined %>%
  mutate(populacoes = recode(populacoes,
                             "POP_LIBER" = "PPL",
                             "POP_RUA" = "PSR",
                             "POP_SAUDE" = "ProfSau",
                             "POP_IMIG" = "Imi"))  # Recode para renomear as populações.

write.csv(data_combined, 'am_combined_populacoes.csv', row.names = F)  # Exporta os resultados para um arquivo CSV.

#------teste - uma pop por vez----------
#------- Pacotes-------
library(tidyverse)
library(dplyr)
library(hash)
library(broom)

source("scripts/data_cleaning.R")
source("scripts/format_p_values.R")

#---------- Carregamento dos dados ----
tb_filtrado = read.csv("TUBEBR/tb_filtrado.csv") |> 
  data_cleaning()

#----------- Regressão Logística----------
model_lm <- function(df, ep, coluna) {
  coluna_sym <- ensym(coluna)  # Converte o nome da coluna em símbolo.
  
  # Filtra para a evolução de interesse e mantém apenas casos completos das populações
  tb_form <- df %>%
    dplyr::filter(EXTRAPU1_N == ep | EXTRAPU1_N == 'Pulmonar') %>%
    mutate(EXTRAPU1_N = fct_relevel(EXTRAPU1_N, "Pulmonar", ep)) %>%
    dplyr::filter(!is.na(!!coluna_sym))  # Mantém apenas linhas onde a coluna não é NA.
  
  # Construa a fórmula dinamicamente
  formula <- as.formula(paste("EXTRAPU1_N ~", coluna))  # Cria a fórmula para o modelo.
  
  # Modelo de regressão logística
  modelo <- glm(formula, data = tb_form, family = binomial())  # Ajusta o modelo.
  
  return(modelo)  # Retorna o modelo ajustado.
}

# Coletar dados do modelo
processar_dados <- function(df, ep, modelo_fn, coluna) {
  modelo <- modelo_fn(df, ep, coluna)  # Chama a função do modelo.
  
  df_estatistics <- tidy(modelo, conf.int = TRUE, exponentiate = TRUE)  # Organiza os resultados do modelo.
  
  # Ajusta a extração para corresponder corretamente às populações
  or <- df_estatistics$estimate[2:5]  # Extrai ORs.
  p <- format_p_values(df_estatistics$p.value[2:5])  # Formata os p-valores.
  
  # Cria um data.frame com as linhas corretas
  resultados <- data.frame(
    populacoes = rep(coluna, length(or)),  # Nome da população.
    OR = or,  # Razões de chances.
    p_value = p,  # P-valores.
    Evolucao = rep(ep, length(or))  # Evolução correspondente.
  )
  
  return(resultados)  # Retorna os resultados.
}

# Função principal
main <- function(df, ep) {
  colunas_pop <- c("POP_LIBER", "POP_RUA", "POP_SAUDE", "POP_IMIG")  # Nomes das populações.
  resultados <- data.frame(populacoes = character(), OR = double(),
                           p_value = character(), Evolucao = character(), stringsAsFactors = FALSE)  # Data frame vazio para resultados.
  
  for (coluna in colunas_pop) {
    ep_df <- processar_dados(df, ep, model_lm, coluna)  # Processa os dados para cada população.
    resultados <- rbind(resultados, ep_df)  # Adiciona os resultados ao data frame.
  }
  
  return(resultados)  # Retorna os resultados combinados.
}

# Criar dataframe com os resultados
df_function <- function(ep_df, ep) {
  ep_df <- data.frame(
    'populacoes' = c("POP_LIBER", "POP_RUA", "POP_SAUDE", "POP_IMIG"),
    'OR' = ep_df[[1]],
    'p_value' = ep_df[[2]],
    'Evolucao' = rep(ep, 4)
  )
  return(ep_df)  # Retorna o data frame com os resultados.
}

# Análises para as evoluções
eps = c("Pleural", "Gang. Perif.", "Geniturinária", "Óssea", "Ocular", "Miliar", 
        "Meningoencefálico", "Cutânea", "Laringea", "Outra", "Pulmonar")

# Executa a função principal para cada evolução
am_pleural <- main(tb_filtrado, eps[1])
am_gang.perif <- main(tb_filtrado, eps[2])
am_genit <- main(tb_filtrado, eps[3])
am_ossea <- main(tb_filtrado, eps[4])
am_ocular <- main(tb_filtrado, eps[5])
am_miliar <- main(tb_filtrado, eps[6])
am_mtb <- main(tb_filtrado, eps[7])
am_cutan <- main(tb_filtrado, eps[8])
am_laring <- main(tb_filtrado, eps[9])

#---- Exportar resultado da análise multivariada ----
data_combined <- bind_rows(
  am_cutan_est,
  am_gang.perif_est,
  am_genit_est, 
  am_laring_est, 
  am_miliar_est, 
  am_mtb_est,
  am_ocular_est, 
  am_ossea_est,
  am_pleural_est
)  # Combina todos os resultados em um único data frame.

data_combined <- data_combined %>%
  mutate(populacoes = recode(populacoes,
                             "POP_LIBER" = "PPL",
                             "POP_RUA" = "PSR",
                             "POP_SAUDE" = "ProfSau",
                             "POP_IMIG" = "Imi"))  # Recode para renomear as populações.

write.csv(data_combined, 'am_combined_populacoes.csv', row.names = F)  # Exporta os resultados para um arquivo CSV.
