# Funções de leitura dos cubos de dados
#
# Os dois cubos são gerados por data-raw/dados_cubos.R:
#   * cubo da INTERVENÇÃO (dados_ts_painel, dados_ts_categorias): abaixo, parte 1
#   * cubo do DESCRITIVO  (dados_desc_painel, dados_desc_niveis): abaixo, parte 2

# PARTE 1 - CUBO DA INTERVENÇÃO

#' Funções de leitura do cubo da intervenção
#'
#' @description O "cubo" são duas tabelas de contagens mensais geradas por
#' `data-raw/dados_cubos.R` (parte 1):
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

# Meses do calendário do SINASC 
# Mesma expressão que os gráficos e os modelos já usam para montar `st`: do
# primeiro mês do SINASC até o último mês menos 1. Assim toda série do cubo
# tem o mesmo tamanho que o calendário dos gráficos.
meses_sinasc <- function() {
  seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
           to   = as.Date(max(dados_sinasc_intervencao$data_variable)) - months(1),
           by   = "1 month")
}

# Alinha um data.frame (DATA, total_casos) aos meses do SINASC, com zero onde faltar 
serie_meses <- function(x) {
  n <- x$total_casos[match(meses_sinasc(), x$DATA)]
  n[is.na(n)] <- 0
  as.numeric(n)
}

# Série geral de um indicador em um local (vetor numérico, um valor por mês)
serie_ts <- function(indicador, nivel = "PR", local = "PR") {
  x <- dados_ts_painel[dados_ts_painel$indicador == indicador &
                         dados_ts_painel$nivel == nivel &
                         dados_ts_painel$local == local, ]
  serie_meses(x)
}

# Contagens por categoria (todas as dimensões) de um indicador em um local
# Colunas: DATA, dimensao, categoria, total_casos
ts_categorias <- function(indicador, nivel = "PR", local = "PR") {
  x <- dados_ts_categorias[dados_ts_categorias$indicador == indicador &
                             dados_ts_categorias$nivel == nivel &
                             dados_ts_categorias$local == local, ]
  x[, c("DATA", "dimensao", "categoria", "total_casos")]
}

# Total de casos de uma dimensão (ou de uma categoria dela) no cubo de categorias
total_casos_cat <- function(dados, dimensao, categoria = NULL) {
  x <- dados[dados$dimensao == dimensao, ]
  if (!is.null(categoria)) {
    x <- x[x$categoria == categoria, ]
  }
  sum(x$total_casos)
}


# PARTE 2 - CUBO DO DESCRITIVO

#' Funções de leitura do cubo do descritivo
#'
#' * `dados_desc_painel`: `indicador | variavel | nivel | local | DATA | categoria | n`
#' * `dados_desc_niveis`: `indicador | variavel | categoria | ordem`
#'
#' São contagens mensais por categoria. As funções abaixo somam os meses até a
#' data de intervenção ("antes") e depois dela ("depois").
#'
#' @noRd

# Índice do cubo
.cubo_desc <- new.env()

linhas_desc <- function(indicador, variavel, nivel, local) {
  if (is.null(.cubo_desc$indice)) {
    .cubo_desc$indice <- split(
      seq_len(nrow(dados_desc_painel)),
      paste(dados_desc_painel$indicador, dados_desc_painel$variavel,
            dados_desc_painel$nivel, dados_desc_painel$local, sep = "|"))
  }
  dados_desc_painel[.cubo_desc$indice[[paste(indicador, variavel, nivel, local, sep = "|")]], ]
}

# Contagens por período e categoria de uma variável em um local
#
# @param indicador "SINASC", "SIM_Neonatal", "SIM_Materno", "SIF_Gestante" ou "SIF_Congenita"
# @param variavel nome da variável (coluna do dataset do descritivo)
# @param nivel,local nível geográfico e local ("PR"/"PR", "macro"/"LESTE", ...)
# @param data_corte data de intervenção escolhida (o mês dela inteiro fica em "antes")
# @param data_inicio datas do SINASC (só o mínimo é usado, para o rótulo do 1º período)
#
# @return data.frame: intervation_date (texto do período), y (categoria, fator na
#   ordem do gráfico; NA é a categoria "vazio"), n, freq (= n) e porcent (% dentro do período)
contagem_desc <- function(indicador, variavel, nivel = "PR", local = "PR",
                          data_corte, data_inicio) {
  x <- linhas_desc(indicador, variavel, nivel, local)

  corte      <- as.Date(lubridate::floor_date(as.Date(data_corte), "month"))
  mes_corte  <- format(as.Date(data_corte), format = "%B/%Y")
  mes_inicio <- format(min(as.Date(data_inicio)), format = "%B/%Y")

  x$intervation_date <- ifelse(x$DATA > corte,
                               paste("Depois de", mes_corte),
                               paste("De", mes_inicio, "a", mes_corte))

  r <- stats::aggregate(n ~ intervation_date + categoria, data = x, FUN = sum)

  # Ordem das categorias no eixo x (a do fator original); "NA" vira NA, que o
  # ggplot põe no fim, como no gráfico atual
  niv <- dados_desc_niveis[dados_desc_niveis$indicador == indicador &
                             dados_desc_niveis$variavel == variavel, ]
  niv <- niv[order(niv$ordem), ]
  r$y <- factor(ifelse(r$categoria == "NA", NA, r$categoria),
                levels = setdiff(niv$categoria, "NA"))
  r <- r[order(r$intervation_date, as.integer(r$y)), ]   # NA fica por último

  r$freq    <- r$n
  r$porcent <- stats::ave(r$n, r$intervation_date, FUN = function(v) v / sum(v) * 100)
  r[, c("intervation_date", "y", "n", "freq", "porcent")]
}

# Estilo do gráfico
estilo_cubo <- function() {
  if (is.null(.cubo_desc$estilo)) {
    .cubo_desc$estilo <- list(
      tema = ggplot2::theme_light() +
        ggplot2::theme(axis.text  = ggplot2::element_text(size = 15),
                       axis.title = ggplot2::element_text(size = 15)),
      cor  = ggplot2::scale_fill_manual(values = c("#6BAED6", "#fc9272"), na.value = "white"),
      y    = ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE)))
  }
  .cubo_desc$estilo
}

# Gráfico de barras a partir das contagens do cubo
plot.col1_cubo <- function(contagem, titulo, legenda, posicao_legenda = "none") {
  if (sum(contagem$n) < 30) {
    return(invisible(NULL))
  }

  estilo  <- estilo_cubo()
  periodo <- sort(unique(contagem$intervation_date))
  cats    <- levels(droplevels(contagem$y))
  if (anyNA(contagem$y)) cats <- c(cats, "NA")

  i <- match(ifelse(is.na(contagem$y), "NA", as.character(contagem$y)), cats)
  j <- match(contagem$intervation_date, periodo)

  n_barras <- tabulate(i, nbins = length(cats))[i]
  ordem    <- stats::ave(j, i, FUN = rank)
  larg     <- 0.9 / n_barras
  xmin     <- i - 0.45 + (ordem - 1) * larg

  barras <- data.frame(
    xmin    = xmin,
    xmax    = xmin + larg,
    freq    = contagem$freq,
    periodo = contagem$intervation_date,
    rotulo  = paste0(round(contagem$porcent, 2), "%")
  )
  barras$xmeio <- (barras$xmin + barras$xmax) / 2

  ggplot2::ggplot(barras) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = freq, fill = periodo)) +
    ggplot2::geom_text(ggplot2::aes(x = xmeio, y = freq, label = rotulo), vjust = -1) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(cats),
      labels = cats,
      limits = c(0.4, length(cats) + 0.6),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = legenda, y = "Frequência", title = titulo, fill = "Período") +
    estilo$cor +
    estilo$y +
    estilo$tema +
    ggplot2::theme(
      legend.position = posicao_legenda,
      panel.grid.minor.x = ggplot2::element_blank()
    )
}
