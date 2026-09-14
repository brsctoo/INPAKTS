#' map_theme
#'
#' @param legend.position Posição da legenda no gráfico.
#' Aceita os mesmos valores de `ggplot2::theme(legend.position = ...)`,
#' como "right", "bottom", "left", "top" ou "none".
#'
#' @return Um objeto de tema do ggplot2 (classe "theme"), para ser somado
#'   (`+`) a um objeto ggplot.
#'
#' @noRd
map_theme <- function(legend.position = "bottom") {
  ggplot2::theme(
    legend.position = legend.position,
    text = ggplot2::element_text(size = 16),
    legend.title = ggplot2::element_text(size = 16, face = "bold"),
    legend.text = ggplot2::element_text(size = 13)
  )
}

#' RSmapOrd
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' Map of a Health Center (RS - REgional de Saúde) in the state of PR according some categorical information
#'
#' @param plot.action True or False for plotting the graphic. The graph object is returned anyway.
#' @param varToPlot Categorical Information to be plotted
#' @param mun Municipality names
#' @param RS Center Health number (Character)
#' @param legeName Name for the legend
#' @param legeLabels Labels for the legend
#' @importFrom magrittr "%>%"
#'
#' @export

RSmapOrd <- function(
  varToPlot,
  legeName,
  mun,
  RS,
  legeLabels,
  plot.action = TRUE
){

  # --------------------------------------------------------------------------
  # 1. PREPARAÇÃO DE DADOS GEOGRÁFICOS
  # --------------------------------------------------------------------------

  # Junta os dados (Aumentou/Diminuiu) com a base de municípios para obter os códigos do IBGE
  data0= dplyr::full_join(
    data.frame(MUNICIPIO=mun,Freq = varToPlot),
    dengueControl::munic
  )

   # Filtra apenas a coluna de interesse e o código do IBGE para o mapa
  data1=data0 %>% dplyr::rename(codigo_ibg = ID_MUNICIP) %>%
    dplyr::select(Freq,codigo_ibg)

  # Cruza os códigos do IBGE com o shapefile
  mapa1=sp::merge(dengueControl::pr_mun,data1,by="codigo_ibg")

  # --------------------------------------------------------------------------
  # 2. CONVERSÃO E FILTRO DE REGIÃO
  # --------------------------------------------------------------------------

  # Extrai as coordenadas para uso no mapa
  mapa1$lon=sp::coordinates(mapa1)[,1]
  mapa1$lat=sp::coordinates(mapa1)[,2]

  # Cria o texto interativo que apareceria ao passar o mouse
  mapa1$legenda=paste(
    mapa1$nome,"<br>","Cluster:",mapa1$Freq
  )

  # Converte o objeto 'sp' (antigo) para 'sf'
  mapa1 <- sf::st_as_sf(mapa1)

  # Filtra o mapa para exibir apenas os polígonos pertencentes à Regional de Saúde (RS) escolhida
  mapa2 <- mapa1[as.character(mapa1$NUMEROREGSAUDE)==RS,]

  # --------------------------------------------------------------------------
  # 3. RENDERIZAÇÃO DO MAPA (ESTILIZAÇÃO E CORES)
  # --------------------------------------------------------------------------

  myCol = c("#fc9272", "#6BAED6", "#FFFFCC") # Laranja, Azul, Amarelo
  myCol1 = c("#fc9272", "#FFFFCC") # Laranja, Amarelo

  if(length(levels(as.factor(varToPlot))) == 3) {
    # Caso 1: 3 categorias (Ex: Aumentou, Diminuiu, Estável)
    pal = colorRampPalette(myCol)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(
        values = pal(length(levels(varToPlot))),
        name=legeName,
        na.value="white",
        labels = legeLabels
      ) +
      # ggplot2::geom_sf(ggplot2::aes(fill=factor(Freq,levels=levels(varToPlot),
      #                                           labels=legeLabels))) +
      # #ggplot2::geom_sf_label(aes(label = nome), label.padding = unit(1, "mm")) +
      # ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
      #                            name=legeName,
      #                            na.value="white") +
      map_theme("bottom")

  } else if(length(levels(as.factor(varToPlot))) == 2) {
    # Caso 2: 2 categorias apenas
    pal = colorRampPalette(myCol1)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(
        values = pal(length(levels(varToPlot))),
        name=legeName,
        na.value="white",
        labels = legeLabels
      ) +
      # ggplot2::geom_sf(ggplot2::aes(fill=factor(Freq,levels=levels(varToPlot),
      #                                           labels=legeLabels))) +
      # #ggplot2::geom_sf_label(aes(label = nome), label.padding = unit(1, "mm")) +
      # ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
      #                            name=legeName,
      #                            na.value="white") +
      map_theme("bottom")


  } else {
    # Caso 3: Fallback
    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=factor(Freq,levels=levels(varToPlot),labels=legeLabels))) +
      ggplot2::scale_fill_brewer(
        name=legeName,type="seq",palette="YlOrRd",
        na.value="white",labels = legeLabels
      ) +
      map_theme("bottom")
  }

  # Imprime o gráfico na tela
  if(plot.action) print(map)

  # Retorna o objeto do mapa de forma invisível para uso interno do Shiny
  invisible(list(map=map))#,cluster.distr=cluster.distr))

}


#' UFmapOrd
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' Map of UF according some ordinal information
#'
#' @param plot.action True or False for plotting the graphic. The graph object is returned anyway.
#' @param varToPlot Ordinal Information to be plotted
#' @param mun Municipality names
#' @param legeName Name for the legend
#' @param legeLabels Labels for the legend
#' @importFrom magrittr "%>%"
#'
#' @export

UFmapOrd <- function(
  varToPlot,
  legeName,
  mun,
  legeLabels,
  plot.action=TRUE
) {

  # --------------------------------------------------------------------------
  # 1. PREPARAÇÃO DE DADOS GEOGRÁFICOS
  # --------------------------------------------------------------------------

  # Junta os dados categóricos com a base de municípios para obter os códigos do IBGE
  data0= dplyr::full_join(
    data.frame(MUNICIPIO=mun,Freq = varToPlot),
    dengueControl::munic
  )

  # Filtra apenas a coluna de interesse e o código do IBGE para o mapa
  data1=data0 %>% dplyr::rename(codigo_ibg = ID_MUNICIP) %>%
    dplyr::select(Freq,codigo_ibg)

  # Cruza os códigos do IBGE com o shapefile do Paraná
  mapa1=sp::merge(dengueControl::pr_mun,data1,by="codigo_ibg")

  # --------------------------------------------------------------------------
  # 2. CONVERSÃO
  # --------------------------------------------------------------------------

  # Extrai as coordenadas para uso no mapa
  mapa1$lon=sp::coordinates(mapa1)[,1]
  mapa1$lat=sp::coordinates(mapa1)[,2]

  # Cria o texto interativo da legenda para a tooltip
  mapa1$legenda=paste(
    mapa1$nome,"<br>", "Cluster:",mapa1$Freq
  )

  # Converte para formato Simple Features (sf) necessário para o geom_sf
  mapa2 <- sf::st_as_sf(mapa1)

  # --------------------------------------------------------------------------
  # 3. RENDERIZAÇÃO DO MAPA
  # --------------------------------------------------------------------------

  myCol = c("#fc9272", "#6BAED6", "#FFFFCC")
  myCol1 = c("#fc9272", "#FFFFCC")

  # Estrutura para coloração baseada no número de categorias
  if(length(levels(as.factor(varToPlot))) == 3) {
    pal = colorRampPalette(myCol)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(
        values = pal(length(levels(varToPlot))),
        name=legeName,
        na.value="white",
        labels = legeLabels
      ) +
      map_theme("bottom")
  } else if(length(levels(as.factor(varToPlot))) == 2) {
    pal = colorRampPalette(myCol1)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(
        values = pal(length(levels(varToPlot))),
        name=legeName,
        na.value="white",
        labels = legeLabels
      ) +
      map_theme("bottom")
  } else {

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_brewer(
        name=legeName,type="seq",palette="YlOrRd",
        na.value="white",labels = legeLabels
      ) +
      map_theme("bottom")
  }

  if(plot.action) print(map)

  invisible(list(map=map))#,cluster.distr=cluster.distr))
}
