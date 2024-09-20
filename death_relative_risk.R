# Carregar pacotes necessários
library(dplyr)      # Pacote para manipulação de dados (parte do tidyverse)
library(tidyverse)  # Conjunto de pacotes para ciência de dados (inclui dplyr, tidyr, ggplot2, etc.)
library(epitools)   # Pacote para calcular medidas epidemiológicas, como risco relativo (RR) e odds ratio (OR)

# Carregar scripts externos com funções de limpeza e formatação
source("scripts/data_cleaning.R")
source("scripts/format_p_values.R")

#---------- Carregamento dos dados ----
# Lê o arquivo CSV com os dados e aplica a função de limpeza (definida em 'data_cleaning.R')
tb_filtrado = read.csv("TUBEBR/tb_filtrado.csv") |> 
  data_cleaning()

#----------- Função para selecionar subpopulações e evoluções ----------
# Filtra os dados de acordo com a população de interesse e evolução extrapulmonar (ep)
pop_select_desf <- function(df, ep, coluna){
  # Filtra os casos onde a população específica está marcada como "Sim"
  pop <- df %>%
    dplyr::filter(!!sym(coluna) == "Sim")
  
  # Filtra os casos com a evolução extrapulmonar de interesse (ep)
  tb_form <- pop |> 
    dplyr::filter(EXTRAPU1_N == ep)
  
  # Reordena os níveis do fator 'EXTRAPU1_N', colocando 'Pulmonar' como referência
  tb_form <- tb_form |>
    mutate(EXTRAPU1_N = fct_relevel(EXTRAPU1_N, "Pulmonar", ep))
  
  return(tb_form)  # Retorna o dataframe filtrado
}

#---------- Seleção e transformação dos dados para cura e óbito ----------
# Filtra os casos onde a situação final é "Cura" ou "Óbito por TB" e reordena esses fatores
Cura_Obito <- tb_filtrado %>%
  filter(SITUA_ENCE == 'Cura' || SITUA_ENCE == 'Óbito por TB') %>%
  mutate(SITUA_ENCE = fct_relevel(SITUA_ENCE, "Cura", "Óbito por TB"))  # Define "Cura" como referência

#----------- Função para calcular o risco relativo (RR) ----------
# Calcula o risco relativo para diferentes comorbidades
calcular_rr <- function(pop) {
  agravos <- c("AGRAVAIDS", "AGRAVALCOO", "AGRAVDIABE", "AGRAVDOENC", "AGRAVDROGA", "AGRAVTABAC")
  resultados <- list("AGRAVAIDS"='', "AGRAVALCOO"='', "AGRAVDIABE"='', 
                     "AGRAVDOENC"='', "AGRAVDROGA"='', "AGRAVTABAC"='')
  
  # Para cada agravo, calcula o risco relativo, intervalo de confiança e p-valor
  for (agravo in agravos) {
    rr <- riskratio(pop$SITUA_ENCE, pop[[agravo]])$measure[2]  # Calcula RR para o agravo
    p_valor <- riskratio(pop$SITUA_ENCE, pop[[agravo]])$p.value[2,2]  # Extrai p-valor
    ci_lower <- riskratio(pop$SITUA_ENCE, pop[[agravo]])$measure[2,2]  # Limite inferior do intervalo de confiança
    ci_upper <- riskratio(pop$SITUA_ENCE, pop[[agravo]])$measure[2,3]  # Limite superior do intervalo de confiança
    resultados[[agravo]] <- c(RR = rr, p_valor = p_valor, lower = ci_lower, upper = ci_upper)  # Salva os resultados
  }
  
  return(resultados)  # Retorna uma lista com os resultados de RR, p-valor e IC para cada agravo
}

#---------- Função para gerar tabela de RRs por comorbidade e população ----------
gerar_tabela_rr <- function(df, eps, colunas_pop) {
  # Cria um dataframe vazio para armazenar os resultados
  resultados_finais <- data.frame(
    agravos = character(),
    PPL_rr = numeric(), PPL_lower = numeric(), PPL_upper = numeric(),
    PSR_rr = numeric(), PSR_lower = numeric(), PSR_upper = numeric(),
    ProfSau_rr = numeric(), ProfSau_lower = numeric(), ProfSau_upper = numeric(),
    Imi_rr = numeric(), Imi_lower = numeric(), Imi_upper = numeric(),
    PPL_p = character(), PSR_p = character(), ProfSau_p = character(), Imi_p = character(),
    Evolucao = character(), stringsAsFactors = FALSE
  )
  
  # Mapeamento de nomes amigáveis para os agravos
  nomes_agravos <- c(
    "AGRAVAIDS" = "AIDS", "AGRAVALCOO" = "Alcool", "AGRAVDOENC" = "Doença\nMental",
    "AGRAVDIABE" = "DBT", "AGRAVTABAC" = "TBG", "AGRAVDROGA" = "Drogas"
  )
  
  # Itera sobre as evoluções e comorbidades para calcular os resultados
  for (ep in eps) {
    for (agravo in names(nomes_agravos)) {
      row_resultados <- list(agravos = nomes_agravos[agravo])
      
      # Itera sobre cada população (PPL, PSR, ProfSau, Imi)
      for (i in seq_along(colunas_pop)) {
        coluna_pop <- colunas_pop[i]
        tb_form <- pop_select_desf(df, ep, coluna_pop)
        
        if (nrow(tb_form) == 0) {
          # Se não houver dados para a população, preenche com NA
          row_resultados[[paste0(colunas_pop_nomes[i], "_rr")]] <- NA
          row_resultados[[paste0(colunas_pop_nomes[i], "_lower")]] <- NA
          row_resultados[[paste0(colunas_pop_nomes[i], "_upper")]] <- NA
          row_resultados[[paste0(colunas_pop_nomes[i], "_p")]] <- NA
          next
        }
        
        # Calcula RR, p-valor e intervalo de confiança para o agravo
        rr_pvalores <- calcular_rr(tb_form)
        rr <- rr_pvalores[[agravo]]["RR"]
        p_valor <- rr_pvalores[[agravo]]["p_valor"]
        ci_lower <- rr_pvalores[[agravo]]["lower"]
        ci_upper <- rr_pvalores[[agravo]]["upper"]
        
        # Formata o p-valor usando a função externa 'format_p_values'
        p_valor_formatado <- format_p_values(p_valor)
        
        # Armazena os resultados
        row_resultados[[paste0(colunas_pop_nomes[i], "_rr")]] <- rr
        row_resultados[[paste0(colunas_pop_nomes[i], "_lower")]] <- ci_lower
        row_resultados[[paste0(colunas_pop_nomes[i], "_upper")]] <- ci_upper
        row_resultados[[paste0(colunas_pop_nomes[i], "_p")]] <- p_valor_formatado
      }
      
      # Adiciona o resultado da evolução ao dataframe final
      row_resultados[["Evolucao"]] <- ep
      resultados_finais <- rbind(resultados_finais, row_resultados)
    }
  }
  
  return(resultados_finais)  # Retorna o dataframe final com os resultados
}

# Definir as evoluções extrapulmonares e as colunas de populações
eps = c("Pleural", "Gang. Perif.", "Geniturinária", "Óssea", "Ocular", "Miliar", "Meningoencefálico", "Cutânea", "Laringea")
colunas_pop <- c("POP_LIBER", "POP_RUA", "POP_SAUDE", "POP_IMIG")

# Gera os resultados e exporta para CSV
resultados <- gerar_tabela_rr(Cura_Obito, eps, colunas_pop)
write.csv(resultados, 'ab_rr_obito_desf.csv', row.names = F)

#----------- Cálculo de risco relativo nas populações específicas ----------
# Função para calcular RR, p-valor e IC para uma população específica
calcular_rr_populacao <- function(pop, populacao_var) {
  rr_result <- riskratio(pop$SITUA_ENCE, pop[[populacao_var]])$measure[2]  # Calcula RR
  p_valor <- riskratio(pop$SITUA_ENCE, pop[[populacao_var]])$p.value[2, 2]  # Extrai p-valor
  ci_lower <- riskratio(pop$SITUA_ENCE, pop[[populacao_var]])$measure[2, 2]  # Limite inferior do IC
  ci_upper <- riskratio(pop$SITUA_ENCE, pop[[populacao_var]])$measure[2, 3]  # Limite superior do IC
  return(c(RR = rr_result, p_valor = p_valor, lower = ci_lower, upper = ci_upper))  # Retorna RR e IC
}

# Função para gerar tabela de RR para cada população
gerar_tabela_rr_populacoes <- function(df, eps, colunas_pop) {
  resultados_finais <- data.frame(
    populacoes = character(), PPL_rr = numeric(), PPL_lower = numeric(), PPL_upper = numeric(),
    PSR_rr = numeric(), PSR_lower = numeric(), PSR_upper = numeric(),
    ProfSau_rr = numeric(), ProfSau_lower = numeric(), ProfSau_upper = numeric(),
    Imi_rr = numeric(), Imi_lower = numeric(), Imi_upper = numeric(),
    PPL_p = character(), PSR_p = character(), ProfSau_p = character(), Imi_p = character(),
    Evolucao = character(), stringsAsFactors = FALSE
  )
  
  for (ep in eps) {
    row_resultados <- list(populacoes = ep)
    
    for (i in seq_along(colunas_pop)) {
      coluna_pop <- colunas_pop[i]
      tb_form <- df %>%
        filter(EXTRAPU1_N == ep) %>%
        complete.cases(tb_form[[coluna_pop]])
      
      if (nrow(tb_form) == 0) {
        row_resultados[[paste0(colunas_pop_nomes[i], "_rr")]] <- NA
        next
      }
      
      # Calcula RR para a população
      rr_pvalores <- calcular_rr_populacao(tb_form, coluna_pop)
      rr <- rr_pvalores["RR"]
      p_valor <- rr_pvalores["p_valor"]
      ci_lower <- rr_pvalores["lower"]
      ci_upper <- rr_pvalores["upper"]
      
      row_resultados[[paste0(colunas_pop_nomes[i], "_rr")]] <- rr
      row_resultados[[paste0(colunas_pop_nomes[i], "_lower")]] <- ci_lower
      row_resultados[[paste0(colunas_pop_nomes[i], "_upper")]] <- ci_upper
      row_resultados[[paste0(colunas_pop_nomes[i], "_p")]] <- format_p_values(p_valor)
    }
    
    row_resultados[["Evolucao"]] <- ep
    resultados_finais <- rbind(resultados_finais, row_resultados)
  }
  
  return(resultados_finais)
}

# Gera os resultados e exporta para CSV
resultados_populacoes <- gerar_tabela_rr_populacoes(Cura_Obito, eps, colunas_pop)
write.csv(resultados_populacoes, 'ab_rr_obito_populacoes.csv', row.names = F)

#----------- Contagem de registros de agravos por subpopulação e evolução ----------
# Função para contar registros de subpopulações e agravos por evolução extrapulmonar
count_subpop_agravo <- function(data, forma_extrapulmonar) {
  data_filtrado <- data %>% filter(EXTRAPU1_N == forma_extrapulmonar)
  
  resultado <- array(0, dim = c(12, 4, 2), dimnames = list(
    c("AGRAVAIDS", "Não AGRAVAIDS", "AGRAVALCOO", "Não AGRAVALCOO", "AGRAVDOENC", "Não AGRAVDOENC", 
      "AGRAVDIABE", "Não AGRAVDIABE", "AGRAVTABAC", "Não AGRAVTABAC", "AGRAVDROGA", "Não AGRAVDROGA"),
    c("POP_LIBER", "POP_RUA", "POP_SAUDE", "POP_IMIG"),
    c("Cura", "Óbito por TB")
  ))
  
  agravos <- c("AGRAVAIDS", "AGRAVALCOO", "AGRAVDOENC", "AGRAVDIABE", "AGRAVTABAC", "AGRAVDROGA")
  
  # Preencher a matriz com contagens
  for (i in seq_along(agravos)) {
    agravo <- agravos[i]
    for (populacao in colnames(resultado)) {
      for (situacao in dimnames(resultado)[[3]]) {
        resultado[2 * i - 1, populacao, situacao] <- sum(
          data_filtrado[[agravo]] == "Sim" & 
            data_filtrado[[populacao]] == "Sim" & 
            data_filtrado$SITUA_ENCE == situacao, 
          na.rm = TRUE
        )
        resultado[2 * i, populacao, situacao] <- sum(
          data_filtrado[[agravo]] == "Não" & 
            data_filtrado[[populacao]] == "Sim" & 
            data_filtrado$SITUA_ENCE == situacao, 
          na.rm = TRUE
        )
      }
    }
  }
  
  df_resultado <- as.data.frame.table(resultado, responseName = "Contagem", stringsAsFactors = FALSE)
  df_resultado <- tidyr::pivot_wider(
    df_resultado, names_from = c(Var2, Var3), values_from = Contagem, names_glue = "{Var2}_{Var3}"
  )
  
  colnames(df_resultado)[1] <- "Agravo"
  
  cols <- c("POP_LIBER_Cura", "POP_LIBER_Óbito por TB", "POP_RUA_Cura", "POP_RUA_Óbito por TB", 
            "POP_SAUDE_Cura", "POP_SAUDE_Óbito por TB", "POP_IMIG_Cura", "POP_IMIG_Óbito por TB")
  
  df_resultado <- df_resultado[, c("Agravo", cols)]
  return(df_resultado)
}

# Formas extrapulmonares de interesse
formas_extrapulmonares <- c("Cutânea", "Gang. Perif.", "Geniturinária", "Laringea", 
                            "Meningoencefálico", "Miliar", "Ocular", "Pleural", "Óssea")

# Gerar resultados e exportar para Excel
lista_resultados <- setNames(lapply(formas_extrapulmonares, function(forma) {
  count_subpop_agravo(Cura_Obito, forma)
}), formas_extrapulmonares)

write_xlsx(lista_resultados, "tabela suplementar 2.xlsx")