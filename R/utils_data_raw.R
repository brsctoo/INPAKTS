# utils_data_raw.R
#
# Funções internas de preparo dos dados, usadas pelos scripts de data-raw/.
#
# Objetivo: centralizar em um único lugar a lógica que se repetia entre os
# módulos de pré-processamento (carregamento de arquivos brutos, recodificação
# de variáveis, junções geográficas e cálculo de tendência), evitando que a
# mesma regra de negócio precise ser corrigida em vários arquivos ao mesmo
# tempo.

#' Padroniza valores "N.I." para "Ignorado"
#'
#' @param dados Data.frame a ser modificado
#' @param colunas Vetor de strings com os nomes das colunas
#'
#' @return Data.frame modificado
#'
#' @noRd
padroniza_ignorado <- function(dados, colunas) {
  dados %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(colunas), ~ forcats::fct_recode(as.factor(.), "N.I." = "Ignorado")))
}

#' Carrega e padroniza a tabela de municípios do Paraná
#'
#' @return Um `data.frame` com as colunas `ibge_estabelecimento`, `municipio`,
#' `micro`, `macro`, `municipio_semacento` e `populacao`.
#'
#' @noRd
carregar_munic <- function() {
  load("data/munic.RData")

  names(munic) <- c(
    "ibge_estabelecimento",
    "municipio",
    "micro",
    "macro",
    "municipio_semacento",
    "populacao"
  )

  munic$ibge_estabelecimento <- as.numeric(munic$ibge_estabelecimento)

  return(munic)
}


#' Classifica uma data como "Antes" ou "Depois" da intervenção
#'
#' @param data Vetor de datas (ou objeto coercível a `Date` na comparação).
#' @param corte Data de corte, como string `"YYYY-MM-DD"`. Padrão: `"2020-03-20"`.
#'
#' @return Vetor character com valores `"Antes"` ou `"Depois"`.
#'
#' @noRd
marcar_periodo_intervencao <- function(data, corte = "2020-03-20") {
  ifelse(data > corte, "Depois", "Antes")
}


#' Recodifica Raça/Cor a partir do código numérico dos DBFs do SINAN
#'
#' @param x Vetor com o código numérico de `CS_RACA` (pode vir como
#'   character/factor; a função converte internamente).
#'
#' @return Vetor character com `"Branca"`, `"Não-branca"`, `"Ignorado"` ou `NA`.
#'
#' @noRd
recodifica_raca_dbf <- function(x) {
  x <- as.numeric(as.character(x))

  dplyr::case_when(
    is.na(x) ~ "Ignorado",
    x == 1 ~ "Branca",
    x > 1 & x < 6 ~ "Não-branca",
    x == 9 ~ "Ignorado",
    TRUE ~ NA_character_
  )
}


#' Reagrupa Raça/Cor já textual (SIM/SINASC) em Branca / Não branca
#'
#' @param x Fator (ou vetor coercível a fator) já com os rótulos textuais de
#' Raça/Cor do SIM/SINASC.
#'
#' @return Fator recodificado com níveis `"Branca"` e `"Não branca"`.
#'
#' @noRd
recodifica_raca_cor <- function(x, incluir_nao_informado = FALSE) {
  mapeamento <- c(
    "Branca" = "Branca",
    "Não branca" = "Preta",
    "Não branca" = "Amarela",
    "Não branca" = "Indígena",
    "Não branca" = "Parda"
  )

  if (incluir_nao_informado) {
    mapeamento <- c(mapeamento, "Não informado" = "N.I.")
  }

  do.call(forcats::fct_recode, c(list(x), as.list(mapeamento)))
}


#' Classifica idade em faixas etárias
#'
#' @param idade Vetor numérico com a idade (em anos).
#' @param tipo Qual esquema de faixa aplicar: `"default"` (10-14, 15-19,
#' 20-39, 40-59) ou `"jovem_adulto_idoso"` (Jovens, Adultos Jovens,
#' Adultos, Idosos).
#'
#' @return Vetor character com a faixa etária correspondente.
#'
#' @noRd
classifica_faixa_etaria <- function(idade, tipo = c("default", "jovem_adulto_idoso")) {
  tipo <- match.arg(tipo)

  if (tipo == "default") {
    dplyr::case_when(
      is.na(idade) ~ "Ignorado",
      idade >= 10 & idade <= 14  ~ "10-14",
      idade >= 15 & idade <= 19  ~ "15-19",
      idade >= 20 & idade <= 39  ~ "20-39",
      idade >= 40 & idade <= 59  ~ "40-59",
      TRUE ~ "Ignorado"
    )
  } else {
    dplyr::case_when(
      idade >= 10 & idade < 19  ~ "Jovens: 10 a 18 anos",
      idade >= 19 & idade < 31  ~ "Adultos Jovens: 19 a 30 anos",
      idade >= 31 & idade <= 60 ~ "Adultos: 31 a 59 anos",
      idade > 60  & idade < 90  ~ "Idosos: acima de 60",
      TRUE ~ NA_character_
    )
  }
}


#' Junta micro/macrorregião a um dataset usando o SINASC como referência
#'
#' @param dados Data.frame a ser enriquecido (ex.: `dados_sim_materno`).
#' @param coluna_municipio Nome (string) da coluna de município em `dados`
#' usada para o join (ex.: `"municipio_obito"`).
#' @param dados_sinasc O dataset `dados_sinasc`, usado como fonte de
#' micro/macrorregião.
#'
#' @return `dados` com as colunas `micro` e `macro` adicionadas e
#' reposicionadas no início do data.frame.
#'
#' @noRd
juntar_geo_sinasc <- function(
  dados,
  coluna_municipio,
  dados_sinasc,
  incluir_municipio_oficial = FALSE
) {
  # Tabela de referência: um município por linha, com sua micro/macrorregião.
  geo_sinasc <- data.frame(
    micro = dados_sinasc$micro,
    macro = dados_sinasc$macro,
    municipio_obito = tolower(dados_sinasc$municipio_semacento)
  )

  if (incluir_municipio_oficial) {
    geo_sinasc$municipio <- dados_sinasc$municipio
  }

  # Lista dos municípios que efetivamente aparecem em `dados`
  munic_ref <- data.frame(municipio_obito = dados[[coluna_municipio]])

  geo <- geo_sinasc %>%
    dplyr::semi_join(munic_ref, by = "municipio_obito") %>%
    dplyr::distinct()

  resultado <- dados %>%
    dplyr::left_join(geo, by = stats::setNames("municipio_obito", coluna_municipio))

  if (incluir_municipio_oficial) {
    resultado %>%
      dplyr::relocate(municipio, micro, macro) %>%
      dplyr::select(-dplyr::all_of(coluna_municipio))
  } else {
    resultado %>%
      dplyr::relocate(micro, macro)
  }
}


#' Carrega e empilha os arquivos brutos do SINASC (2015-2022)
#'
#' @return Um único data.frame com todas as linhas de 2015 a 2022, sem
#'   nenhum tratamento adicional (filtros e recodificações ficam a cargo de
#'   quem chama esta função).
#'
#' @noRd
carregar_sinasc_bruto <- function() {
  sinasc_2015 <- readr::read_delim(unz("data/sinasc-2015-2019.zip", "sinasc-2015.csv"), delim = ";")
  sinasc_2016 <- readr::read_delim(unz("data/sinasc-2015-2019.zip", "sinasc-2016.csv"), delim = ";")
  sinasc_2017 <- readr::read_delim(unz("data/sinasc-2015-2019.zip", "sinasc-2017.csv"), delim = ";")
  sinasc_2018 <- readr::read_delim(unz("data/sinasc-2015-2019.zip", "sinasc-2018.csv"), delim = ";")
  sinasc_2019_2022 <- readr::read_delim(unz("data/SINASC-2019-2022.zip", "SINASC-2019-2022.csv"), delim = ";")

  rbind(sinasc_2015, sinasc_2016, sinasc_2017, sinasc_2018, sinasc_2019_2022)
}


#' Recodifica a variável de número de partos cesáreos anteriores
#'
#' @param x Vetor/fator com os valores originais de `parto_cesarea`.
#'
#' @return Vetor character com `"Nenhum"`, `"Um"`, `"Dois"`, `"Mais que dois"`
#'   ou `"Ignorado"`.
#'
#' @noRd
recodifica_parto_cesarea <- function(x) {
  # "Nenhum" -> "0" e "Não informado" -> "4000"
  x <- forcats::fct_recode(x, "0" = "Nenhum", "4000" = "Não informado")
  x <- as.numeric(as.character(x))

  dplyr::case_when(
    x > 36 ~ "Ignorado",
    x == 0 ~ "Nenhum",
    x == 1 ~ "Um",
    x == 2 ~ "Dois",
    x > 2 ~ "Mais que dois",
    TRUE ~ NA_character_
  )
}


#' Recodifica o mês de início do pré-natal em trimestre gestacional
#'
#' @param x Vetor numérico (ou coercível) com o mês de gestação em que o
#' pré-natal foi iniciado.
#'
#' @return Vetor character com `"1º Trimestre"`, `"2º Trimestre"`,
#' `"3º Trimestre"` ou `"Ignorado"`.
#'
#' @noRd
recodifica_mes_gestacao_prenatal <- function(x) {
  x <- as.numeric(x)

  dplyr::case_when(
    is.na(x) ~ "Ignorado",
    x >= 1  & x < 4 ~ "1º Trimestre",
    x >= 4  & x < 7 ~ "2º Trimestre",
    x >= 7  & x < 11 ~ "3º Trimestre",
    TRUE ~ "Ignorado"
  )
}


#' Tendência mensal por município ou regional de saúde
#'
#' @param data_prep Data.frame já tratado, com `data_variable` (data do
#'   evento) e a coluna indicada em `nivel`, que precisa ser fator.
#' @param nivel `"municipio"` ou `"micro"` (regional de saúde).
#' @param data_primeira_intervencao Primeiro mês testado como ponto de
#'   intervenção, no formato `"YYYY-MM-DD"`.
#'
#' @return Data.frame com uma linha por grupo e uma coluna por mês testado.
#'
#' @noRd
data_prep_geo <- function(
  data_prep,
  nivel = c("municipio", "micro"),
  data_primeira_intervencao = "2015-05-01"
) {
  nivel <- match.arg(nivel)

  # Sequência de meses candidatos a ponto de intervenção
  datas <- seq.Date(
    from = as.Date(data_primeira_intervencao),
    to   = as.Date(max(data_prep$data_variable)) - months(1),
    by   = "1 month"
  )

  n <- length(datas) - 1

  data_inicio_Ano <- as.numeric(format(as.Date(min(data_prep$data_variable)), format = "%Y"))
  data_inicio_Mes <- as.numeric(format(as.Date(min(data_prep$data_variable)), format = "%m"))

  grupos <- levels(data_prep[[nivel]])


  dados_por_grupo <- split(data_prep, data_prep[[nivel]])

  # Uma série mensal por grupo (não depende do mês testado)
  series_list <- lapply(
    dados_por_grupo,
    function(dados) {
      return_ts(dados, data_variable, inicio = c(data_inicio_Ano, data_inicio_Mes))
    }
  )

  # Data.frame de resultado
  df <- data.frame(grupos, stringsAsFactors = FALSE)
  names(df)[1] <- nivel

  trendAntes <- vector("numeric", length(grupos))
  trendChange <- vector("numeric", length(grupos))
  trendChangeCat <- vector("character", length(grupos))

  for (j in 1:n) {
    tictoc::tic(sprintf("data_prep_geo (%s) - Processando mês %d de %d", nivel, j, n))

    for (i in seq_along(grupos)) {

      serie <- series_list[[i]]

      # 1. Ajuste do modelo de intervenção com ponto de quebra no mês j
      modelo <- sinasc_modelo.ajustado(
        dados = serie,
        intervention1 = datas[j],
        intervention2 = NA
      )

      # 2. Tendência prévia
      trendAntes[i] <- modelo$ResultingTrends[1, 1]

      # 3. Mudança de tendência
      trendChange[i] <- modelo$fit_lm$coefficients[3]

      # 4. P-valor da mudança de tendência, pra decidir se ela é significativa
      p_valor <- summary(modelo$fit_lm)$coefficients[3, 4]

      # NA quando não dá pra classificar
      trendChangeCat[i] <- dplyr::case_when(
        is.na(p_valor)         ~ NA_character_,
        p_valor >= 0.05        ~ "Estável",
        is.na(trendChange[i])  ~ NA_character_,
        trendChange[i] > 0     ~ "Aumentou",
        TRUE                   ~ "Diminuiu"
      )
    }

    # Cada mês testado vira uma nova coluna no data.frame de resultado,
    # nomeada com a própria data do ponto de intervenção.
    df[, j + 1] <- as.factor(trendChangeCat)
    names(df)[j + 1] <- as.character(datas[j])

    tictoc::toc()
  }

  return(df)
}


#' Junta um dataset agregado por regional de saúde à tabela de regionais
#'
#' @param df Data.frame com a primeira coluna sendo o código/nome da
#' regional de saúde (tipicamente a saída de
#' `data_prep_geo(nivel = "micro")`).
#'
#' @return `df` com a coluna de regional renomeada para `micro` e uma nova
#' coluna `municipio` trazida de `dengueControl::pr_mun`.
#'
#' @noRd
juntar_regiao_saude <- function(df) {
  names(df)[1] <- "NUMEROREGSAUDE"

  df %>%
    merge(dengueControl::pr_mun[, c("NUMEROREGSAUDE", "nome")]) %>%
    dplyr::rename("micro" = "NUMEROREGSAUDE", "municipio" = "nome")
}
