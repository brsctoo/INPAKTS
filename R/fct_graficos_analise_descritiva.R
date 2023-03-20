#' Função que gera gráficos na aba análise descritiva
#'
#' @description Função que gera gráficos nas 3 abas da análise descritiva
#'
#' @param data_prep um data.frame alterando de acordo com as opções do usuário da plataforma
#' @param x variável  que irá no eixo x
#' @param y ariável  que irá no eixo y
#' @param titulo título que irá no gráfico
#'
#' @return ggplot
#'
#' @details
#' Pode escrever o que realiza a função de forma mais detalhada
#'
#'
#'
#' @noRd
#' @import ggplot2
#' @import dplyr
#'
#' @export

# Função para gerar os graficos----------------------

plot.col <-  function(data_prep,x,y,titulo) {

  colors1 <- c("#fc9272","#6BAED6")

  g <- data_prep %>%

    dplyr::select({{ x }},{{ y }}) %>%

    dplyr::group_by({{ y }},{{ x }}) %>%
    dplyr::summarize(n =  dplyr::n()) %>%
    #mutate(freq = n) %>%
    dplyr::mutate(freq = n / sum(n)) %>%
    #ggplot(aes(x={{ x }}, y = freq, fill={{ y }})) +
    ggplot2::ggplot(ggplot2::aes(x={{ y }}, y = freq, fill={{ x }})) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = titulo,
                  x = "Período",
                  y = "Proporção",
                  fill=titulo)+
    ggplot2::scale_fill_manual(values = colors1,na.value="white")+
    theme_light()+
    ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 15))
  return(g)
}


plot.col1 <- function(data_prep,x,y,titulo, legenda, posicao_legenda="none"){

  colors1 <- c("#6BAED6","#fc9272")

  g <- data_prep %>%
    dplyr::select({{ x }},{{ y }}) %>%
    dplyr::group_by({{ x }},{{ y }}) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::mutate(freq = n ) %>%
    dplyr::mutate(porcent = (n /sum(n))*100) %>%
    ggplot2::ggplot(ggplot2::aes(x={{ y }}, y = freq, fill={{ x }})) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = legenda ,
                  y = "Frequência",
                  title = titulo,
                  fill= "Período")+
    ggplot2::scale_fill_manual(values = colors1,na.value="white")+
    ggplot2::theme_light()+
    ggplot2::theme(axis.text = ggplot2::element_text(size = 15),axis.title = ggplot2::element_text(size = 15)) +
    ggplot2::theme(legend.position=posicao_legenda)+
    geom_text(aes(label =  paste ( round(porcent, 2), "%", sep="")), vjust=-1, position = position_dodge(0.9))+
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

  #Se o banco de dados selecionado pelo user n < 30 então
  if(nrow(data_prep)<30){
    #o gráfico não é printado
    return(invisible(g))
  }else{
    return(g)
  }
}


plot.col1_teste <- function(data_prep_desc,y,titulo, legenda, posicao_legenda="none"){

  colors1 <- c("#6BAED6","#fc9272")

  g <- data_prep_desc %>%
    dplyr::select(intervation_date,{{ y }}) %>%
    dplyr::group_by(intervation_date,{{ y }}) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::mutate(freq = n ) %>%
    dplyr::mutate(porcent = (n /sum(n))*100) %>%
    ggplot2::ggplot(ggplot2::aes(x={{ y }}, y = freq, fill=intervation_date)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = legenda ,
                  y = "Frequência",
                  title = titulo,
                  fill= "Período")+
    ggplot2::scale_fill_manual(values = colors1,na.value="white")+
    ggplot2::theme_light()+
    ggplot2::theme(axis.text = ggplot2::element_text(size = 15),axis.title = ggplot2::element_text(size = 15)) +
    ggplot2::theme(legend.position=posicao_legenda)+
    geom_text(aes(label =  paste ( round(porcent, 2), "%", sep="")), vjust=-1, position = position_dodge(0.9))+
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

  #Se o banco de dados selecionado pelo user n < 30 então
  if(nrow(data_prep_desc)<30){
    #o gráfico não é printado
    return(invisible(g))
  }else{
    return(g)
  }
}

# plot.col1_teste <- function(data_prep,y,titulo, legenda, posicao_legenda="none"){
#
#   colors1 <- c("#6BAED6","#fc9272")
#
#   g <- data_prep %>%
#     dplyr::select(intervation_date,{{ y }}) %>%
#     dplyr::group_by(intervation_date,{{ y }}) %>%
#     dplyr::summarize(n = dplyr::n()) %>%
#     dplyr::mutate(freq = n ) %>%
#     dplyr::mutate(porcent = (n /sum(n))*100) %>%
#     ggplot2::ggplot(ggplot2::aes(x={{ y }}, y = freq, fill=intervation_date)) +
#     ggplot2::geom_col(position = "dodge") +
#     ggplot2::labs(x = legenda ,
#                   y = "Frequência",
#                   title = titulo,
#                   fill= "Período")+
#     ggplot2::scale_fill_manual(values = colors1,na.value="white")+
#     ggplot2::theme_light()+
#     ggplot2::theme(axis.text = ggplot2::element_text(size = 15),axis.title = ggplot2::element_text(size = 15)) +
#     ggplot2::theme(legend.position=posicao_legenda)+
#     geom_text(aes(label =  paste ( round(porcent, 2), "%", sep="")), vjust=-1, position = position_dodge(0.9))
#
#   #Se o banco de dados selecionado pelo user n < 30 então
#   if(nrow(data_prep)<30){
#     #o gráfico não é printado
#     return(invisible(g))
#   }else{
#     return(g)
#   }
# }


#Função para selecionar subamostra do conjunto de dados de acordo com as opões selecionadas pelo usuário ----------------------

data_prep_desc <- function(dados,
                           nivel_geografico = "PR",
                           local = "PR",
                           intervention_date_user,
                           data_inicio){
  #Formato Mes/ano
  intervention_date_user_mesAno <- format(as.Date(intervention_date_user),format = "%B/%Y")
  data_inicio <- format(min(as.Date(data_inicio)),format = "%B/%Y")

  if(nivel_geografico=="PR" & local=="PR"){
    data_grafico <- dados
    data_grafico$intervation_date = ifelse(data_grafico$data_variable > intervention_date_user,
                                           paste("Depois de",intervention_date_user_mesAno),
                                           ifelse(data_grafico$data_variable <= intervention_date_user,
                                                  paste("De",data_inicio,"a",intervention_date_user_mesAno),intervention_date_user_mesAno))


    #dplyr::mutate(
    # intervation_date = ifelse(data_variable > intervention_date_user,
    #                           paste("Depois de",intervention_date_user,
    #                                 ifelse(data_variable <= intervention_date_user,
    #                                        paste("Antes de",intervention_date_user),intervention_date_user))))
  }else{
    data_grafico <- dados
    data_grafico$intervation_date = ifelse(data_grafico$data_variable > intervention_date_user,
                                           paste("Depois de",intervention_date_user_mesAno),
                                           ifelse(data_grafico$data_variable <= intervention_date_user,
                                                  paste("De",data_inicio,"a",intervention_date_user_mesAno),intervention_date_user_mesAno))



    #dplyr::filter(nivel_geografico == local)
    # dplyr::mutate(
    #   intervation_date = ifelse(data_variable > intervention_date_user,
    #                             paste("Depois de",intervention_date_user,
    #                                   ifelse(data_variable <= intervention_date_user,
    #                                          paste("Antes de",intervention_date_user),intervention_date_user))))
  }
  return(data_grafico)
}
