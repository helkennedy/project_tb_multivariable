#------- Pacotes-------
# Carrega pacotes necessários para manipulação de dados, modelagem estatística e exportação de planilhas
library(tidyverse)   # Conjunto de pacotes para ciência de dados, como dplyr, ggplot2, etc.
library(dplyr)       # Manipulação de dataframes (parte do tidyverse)
library(broom)       # Organiza saídas de modelos estatísticos em dataframes "tidy"
library(finalfit)    # Criação de tabelas de regressão e análise estatística
library(openxlsx)    # Manipulação de arquivos Excel

# Carrega scripts externos com funções de limpeza e formatação
source("scripts/data_cleaning.R")
source("scripts/format_p_values.R")

#---------- Data loading ----
# Carrega e limpa os dados do arquivo CSV
tb_filtrado = read.csv("TUBEBR/tb_filtrado.csv") |> 
  data_cleaning()

# Remove registros com valores ausentes na coluna EXTRAPU1_N
tb_filtrado1 <- tb_filtrado[!is.na(tb_filtrado$EXTRAPU1_N),]

#----- Óbitos -----
resultados_lista <- list()  # Inicializa uma lista para armazenar resultados

# Novo vetor com rótulos das categorias
categoria_labels <- c(
  "POP_LIBER" = "PPL", "POP_RUA" = "PSR", "POP_SAUDE" = "ProfSau", "POP_IMIG" = "Imi", 
  "AGRAVAIDS" = "AIDS", "AGRAVALCOO" = "Alcool", "AGRAVDOENC" = "Doença Mental", 
  "AGRAVDIABE" = "Diabetes", "AGRAVTABAC" = "Tabagismo", "AGRAVDROGA" = "Drogas Ilícitas"
)

# Lista de formas extrapulmonares, excluindo "Pulmonar"
formas_extrapulmonares <- unique(tb_filtrado1$EXTRAPU1_N)
formas_extrapulmonares <- formas_extrapulmonares[formas_extrapulmonares != "Pulmonar"]

# Lista das populações de interesse
populacoes <- c("POP_LIBER", "POP_RUA", "POP_SAUDE", "POP_IMIG")

# Loop para realizar análise por cada forma extrapulmonar e população
for (TBEP in formas_extrapulmonares) {
  
  for (POP in populacoes) {
    
    # Filtra os dados para cada combinação de forma extrapulmonar (TBEP) e população (POP)
    resultado <- tb_filtrado1 %>%
      filter(EXTRAPU1_N == TBEP) %>%
      filter(SITUA_ENCE == "Cura" | SITUA_ENCE == "Óbito por TB") %>%
      filter(!!sym(POP) == 'Sim')  # Filtra onde a população está presente (Sim)
    
    # Definição das variáveis explicativas (comorbidades)
    variaveis_explicativas <- c("AGRAVALCOO", "AGRAVDOENC", "AGRAVAIDS", 
                                "AGRAVDIABE", "AGRAVTABAC", "AGRAVDROGA")
    
    # Usa try() para garantir que o código continue mesmo em caso de erro
    resultado <- try({
      resultado %>%
        summary_factorlist(dependent = "SITUA_ENCE",  # Variável dependente (Cura ou Óbito)
                           explanatory = variaveis_explicativas,  # Variáveis explicativas
                           p = TRUE,  # Inclui p-valor
                           p_cat = "fisher") %>%
        select(-starts_with("Laringea"), -starts_with("Pleural"), 
               -starts_with("Miliar"), -starts_with("Gang. Perif."),
               -starts_with("Geniturinária"), -starts_with("Ocular"),
               -starts_with("Cutânea"), -starts_with("Óssea"))  # Remove colunas indesejadas
    })
    
    # Se ocorrer erro, pula para a próxima iteração
    if (inherits(resultado, "try-error")) {
      next
    }
    
    # Renomeia variáveis explicativas com os rótulos definidos em 'categoria_labels'
    resultado <- resultado %>%
      mutate(label = recode(label, !!!categoria_labels))
    
    # Adiciona informações sobre a forma extrapulmonar e a população
    resultado$Forma_Extrapulmonar <- TBEP
    resultado$Populacao <- recode(POP, !!!categoria_labels)
    
    # Adiciona uma linha em branco no final da tabela
    resultado <- resultado %>%
      add_row(.before = NULL)
    
    # Armazena o resultado na lista com chave combinando forma extrapulmonar e população
    resultados_lista[[paste(TBEP, recode(POP, !!!categoria_labels), sep = "_")]] <- resultado
  }
}

# Combina todos os resultados da lista em uma única tabela
resultados_combinados <- bind_rows(resultados_lista, .id = "ID")

# Cria um workbook Excel para armazenar os resultados
wb <- createWorkbook()

# Loop para criar uma aba (sheet) no Excel para cada forma extrapulmonar
for (TBEP in formas_extrapulmonares) {
  
  # Filtra os resultados da forma extrapulmonar atual
  filtro <- resultados_combinados %>%
    filter(Forma_Extrapulmonar == TBEP)
  
  # Adiciona uma nova aba com o nome da forma extrapulmonar
  addWorksheet(wb, TBEP)
  
  # Escreve os dados filtrados na aba correspondente
  writeData(wb, TBEP, filtro)
}

# Salva o arquivo Excel
saveWorkbook(wb, "resultados_extrapulmonares_obito.xlsx", overwrite = TRUE)

#----- casos -----
# Funções para filtrar e processar dados de forma extrapulmonar e população

# Função para filtrar os dados com base na forma extrapulmonar e população
filtrar_dados <- function(tb_filtrado1, TBEP, POP) {
  tb_filtrado1 %>%
    filter(EXTRAPU1_N == TBEP | EXTRAPU1_N == 'Pulmonar') %>%
    mutate(EXTRAPU1_N = as.factor(EXTRAPU1_N)) %>%
    mutate(fct_relevel(EXTRAPU1_N, c('Pulmonar', TBEP))) %>%
    filter(!!sym(POP) == 'Sim')
}

# Função para aplicar a função 'summary_factorlist' e gerar o resultado estatístico
processar_resultado <- function(filtrado, TBEP) {
  filtrado %>%
    summary_factorlist(dependent = "EXTRAPU1_N",  # Variável dependente (forma extrapulmonar)
                       explanatory = c("AGRAVALCOO", "AGRAVDOENC", 
                                       "AGRAVAIDS", "AGRAVDIABE", 
                                       "AGRAVTABAC", "AGRAVDROGA"),
                       p = TRUE,  # Inclui p-valores
                       p_cat = "fisher")  # Teste de Fisher para p-valores
}

# Função para renomear e armazenar os resultados com rótulos adequados
armazenar_resultado <- function(resultado, TBEP, POP, categoria_labels) {
  # Renomeia variáveis explicativas com rótulos definidos
  resultado <- resultado %>%
    mutate(label = recode(label, !!!categoria_labels))
  
  # Adiciona informações sobre a forma extrapulmonar e população
  resultado$Forma_Extrapulmonar <- TBEP
  resultado$Populacao <- recode(POP, !!!categoria_labels)
  
  # Adiciona uma linha em branco no final do resultado
  resultado <- resultado %>%
    add_row(.before = NULL)
  
  # Retorna o resultado processado
  return(resultado)
}

# Função principal que executa loops sobre as formas extrapulmonares e populações
executar_loops <- function(tb_filtrado1, formas_extrapulmonares, populacoes, categoria_labels) {
  resultados_lista <- list()  # Inicializa lista para armazenar resultados
  
  for (TBEP in formas_extrapulmonares) {
    for (POP in populacoes) {
      # Filtra dados
      filtrado <- filtrar_dados(tb_filtrado1, TBEP, POP)
      
      # Processa os dados filtrados
      resultado <- processar_resultado(filtrado, TBEP)
      
      # Armazena o resultado com rótulos corretos
      resultado_final <- armazenar_resultado(resultado, TBEP, POP, categoria_labels)
      
      # Armazena o resultado na lista
      resultados_lista[[paste(TBEP, recode(POP, !!!categoria_labels), sep = "_")]] <- resultado_final
    }
  }
  
  return(resultados_lista)  # Retorna a lista de resultados
}

# Executa a função principal para gerar os resultados
resultados_lista <- executar_loops(tb_filtrado1, formas_extrapulmonares, populacoes, categoria_labels)

# Cria um workbook Excel para armazenar os resultados
wb <- createWorkbook()

# Define o número de elementos por grupo (4 neste caso)
n_por_grupo <- 4

# Loop para agrupar os resultados de 4 em 4 e escrever no Excel
for (i in seq(1, length(resultados_lista), by = n_por_grupo)) {
  # Define o nome da planilha baseado no grupo atual
  sheet_name <- formas_extrapulmonares[ceiling(i / n_por_grupo)]
  
  # Agrupa 4 resultados da lista
  grupo_resultados <- resultados_lista[i:(i + n_por_grupo - 1)]
  
  # Combina os 4 dataframes em um só
  resultado_combinado <- bind_rows(grupo_resultados)
  
  # Adiciona a planilha ao workbook
  addWorksheet(wb, sheet_name)
  
  # Escreve os dados na planilha
  writeData(wb, sheet_name, resultado_combinado)
}

# Salva o arquivo Excel
saveWorkbook(wb, "resultado_final.xlsx", overwrite = TRUE)
