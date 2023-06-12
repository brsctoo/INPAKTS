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

RSmapOrd <- function(varToPlot,
                     legeName,
                     mun,
                     RS,
                     legeLabels,
                     plot.action=TRUE
){


  #---------------------------------------------------------------------------
  # Mapa
  #---------------------------------------------------------------------------

  myCol = c("#fc9272", "#6BAED6", "#FFFFCC")
  myCol1 = c("#fc9272", "#FFFFCC") #Trocar as cores

  data0= dplyr::full_join(data.frame(MUNICIPIO=mun,Freq = varToPlot),
                          dengueControl::munic)

  data1=data0 %>% dplyr::rename(codigo_ibg = ID_MUNICIP) %>%
    dplyr::select(Freq,codigo_ibg)

  mapa1=sp::merge(dengueControl::pr_mun,data1,by="codigo_ibg")

  mapa1$lon=sp::coordinates(mapa1)[,1]
  mapa1$lat=sp::coordinates(mapa1)[,2]
  mapa1$legenda=paste(mapa1$nome,"<br>",
                      "Cluster:",mapa1$Freq)

  mapa1 <- sf::st_as_sf(mapa1)

  mapa2 <- mapa1[as.character(mapa1$NUMEROREGSAUDE)==RS,]


  if(length(levels(as.factor(varToPlot))) == 3) {
    pal = colorRampPalette(myCol)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
                                 name=legeName,
                                 na.value="white",
                                 labels = legeLabels) +
      # ggplot2::geom_sf(ggplot2::aes(fill=factor(Freq,levels=levels(varToPlot),
      #                                           labels=legeLabels))) +
      # #ggplot2::geom_sf_label(aes(label = nome), label.padding = unit(1, "mm")) +
      # ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
      #                            name=legeName,
      #                            na.value="white") +
      ggplot2::theme(legend.position = "right",
                     text = ggplot2::element_text(size = ggplot2::rel(4)),
                     legend.text = ggplot2::element_text(size = ggplot2::rel(2.8)))

  } else if(length(levels(as.factor(varToPlot))) == 2) {
    pal = colorRampPalette(myCol1)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
                                 name=legeName,
                                 na.value="white",
                                 labels = legeLabels) +
      # ggplot2::geom_sf(ggplot2::aes(fill=factor(Freq,levels=levels(varToPlot),
      #                                           labels=legeLabels))) +
      # #ggplot2::geom_sf_label(aes(label = nome), label.padding = unit(1, "mm")) +
      # ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
      #                            name=legeName,
      #                            na.value="white") +
      ggplot2::theme(legend.position = "right",
                     text = ggplot2::element_text(size = ggplot2::rel(4)),
                     legend.text = ggplot2::element_text(size = ggplot2::rel(2.8)))

  } else {

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=factor(Freq,levels=levels(varToPlot),labels=legeLabels))) +
      ggplot2::scale_fill_brewer(name=legeName,type="seq",palette="YlOrRd",
                                 na.value="white",labels = legeLabels) +
      ggplot2::theme(legend.position = "right",
                     text = ggplot2::element_text(size = ggplot2::rel(4)),
                     legend.text = ggplot2::element_text(size = ggplot2::rel(2.8)))
  }


  if(plot.action) print(map)

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

UFmapOrd <- function(varToPlot,
                     legeName,
                     mun,
                     legeLabels,
                     plot.action=TRUE
){


  #---------------------------------------------------------------------------
  # Mapa
  #---------------------------------------------------------------------------

  data0= dplyr::full_join(data.frame(MUNICIPIO=mun,Freq = varToPlot),
                          dengueControl::munic)

  data1=data0 %>% dplyr::rename(codigo_ibg = ID_MUNICIP) %>%
    dplyr::select(Freq,codigo_ibg)

  mapa1=sp::merge(dengueControl::pr_mun,data1,by="codigo_ibg")

  mapa1$lon=sp::coordinates(mapa1)[,1]
  mapa1$lat=sp::coordinates(mapa1)[,2]
  mapa1$legenda=paste(mapa1$nome,"<br>",
                      "Cluster:",mapa1$Freq)

  mapa2 <- sf::st_as_sf(mapa1)

  myCol = c("#fc9272", "#6BAED6", "#FFFFCC") #Trocar as cores
  myCol1 = c("#fc9272", "#FFFFCC") #Trocar as cores

  if(length(levels(as.factor(varToPlot))) == 3) {
    pal = colorRampPalette(myCol)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
                                 name=legeName,
                                 na.value="white",
                                 labels = legeLabels) +
      ggplot2::theme(legend.position = "bottom",#"right"
                     text = ggplot2::element_text(size = ggplot2::rel(4)),
                     legend.text = ggplot2::element_text(size = ggplot2::rel(2.8)))

  } else if(length(levels(as.factor(varToPlot))) == 2) {
    pal = colorRampPalette(myCol1)

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_manual(values = pal(length(levels(varToPlot))),
                                 name=legeName,
                                 na.value="white",
                                 labels = legeLabels) +
      ggplot2::theme(legend.position = "bottom",#"right"
                     text = ggplot2::element_text(size = ggplot2::rel(4)),
                     legend.text = ggplot2::element_text(size = ggplot2::rel(2.8)))

  } else {

    map <- ggplot2::ggplot(mapa2) +
      ggplot2::geom_sf(ggplot2::aes(fill=Freq)) +
      ggplot2::scale_fill_brewer(name=legeName,type="seq",palette="YlOrRd",
                                 na.value="white",labels = legeLabels) +
      ggplot2::theme(legend.position = "bottom",#"right"
                     text = ggplot2::element_text(size = ggplot2::rel(4)),
                     legend.text = ggplot2::element_text(size = ggplot2::rel(2.8)))
  }



  if(plot.action) print(map)

  invisible(list(map=map))#,cluster.distr=cluster.distr))

}







