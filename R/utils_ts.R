#' Funções de leitura do cubo de dados (utils_ts)
#'
#' @description O "cubo" são duas tabelas de contagens mensais geradas por
#' `data-raw/dados_ts.R`:
#'
#' * `dados_ts_painel`: `indicador | nivel | local | DATA | total_casos`
#' * `dados_ts_categorias`: `indicador | nivel | local | DATA | dimensao |
#'   categoria | total_casos`
#'
#' Elas guardam apenas os meses com casos. As funções abaixo devolvem as séries
#' no calendário do SINASC (o mesmo que os gráficos já usam), com zero nos
#' meses sem registro.
#'
#' @noRd

# Meses do calendário do SINASC -------------------------------------------------
# Mesma expressão que os gráficos e os modelos já usam para montar `st`: do
# primeiro mês do SINASC até o último mês menos 1. Assim toda série do cubo
# tem o mesmo tamanho que o calendário dos gráficos.
meses_sinasc <- function() {
  seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
           to   = as.Date(max(dados_sinasc_intervencao$data_variable)) - months(1),
           by   = "1 month")
}

# Alinha um data.frame (DATA, total_casos) aos meses do SINASC, com zero onde faltar --
serie_meses <- function(x) {
  n <- x$total_casos[match(meses_sinasc(), x$DATA)]
  n[is.na(n)] <- 0
  as.numeric(n)
}

# Série geral de um indicador em um local (vetor numérico, um valor por mês) ---------
serie_ts <- function(indicador, nivel = "PR", local = "PR") {
  x <- dados_ts_painel[dados_ts_painel$indicador == indicador &
                         dados_ts_painel$nivel == nivel &
                         dados_ts_painel$local == local, ]
  serie_meses(x)
}

# Contagens por categoria (todas as dimensões) de um indicador em um local ----------
# Colunas: DATA, dimensao, categoria, total_casos
ts_categorias <- function(indicador, nivel = "PR", local = "PR") {
  x <- dados_ts_categorias[dados_ts_categorias$indicador == indicador &
                             dados_ts_categorias$nivel == nivel &
                             dados_ts_categorias$local == local, ]
  x[, c("DATA", "dimensao", "categoria", "total_casos")]
}

# Total de casos de uma dimensão (ou de uma categoria dela) no cubo de categorias ----
total_casos_cat <- function(dados, dimensao, categoria = NULL) {
  x <- dados[dados$dimensao == dimensao, ]
  if (!is.null(categoria)) {
    x <- x[x$categoria == categoria, ]
  }
  sum(x$total_casos)
}
