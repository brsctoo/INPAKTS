#' testando UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_testando_ui <- function(id){
  ns <- NS(id)
  tagList(
    #textOutput(ns("text")),
   # textOutput(ns("text1")),
    #textOutput(ns("text2")),
    #textOutput(ns("text3")),
   # textOutput(ns("text4")),
    textOutput(ns("data1class")),
   textOutput(ns("data1")),
    shinyWidgets::airDatepickerInput(inputId = ns("data2"),
                                     label = "",
                                     language = 'pt-BR',
                                     value = c("2019-02-01","2020-10-01"),
                                     width = "250px",
                                     maxDate = as.Date(max(dados_sinasc_intervencao$data_variable))-months(5),
                                     minDate = min(as.Date(dados_sinasc_intervencao$data_variable)),
                                     view = "months", #editing what the popup calendar shows when it opens
                                     minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                     dateFormat = "MM/yyyy",
                                     clearButton=T,
                                     range = T)


    )
}

#' testando Server Functions
#'
#' @noRd
mod_testando_server <- function(id, opcoes_usuario){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$text <- renderPrint({"olá"})
    # output$text1 <- renderPrint({
    #   opcoes_usuario$date_intervention[1]
    # })
    # output$text2 <- renderPrint({
    #   opcoes_usuario$date_intervention[2]
    # })
    # output$text3 <- renderPrint({
    #   opcoes_usuario$nivel_geografico
    # })
    # output$text4 <- renderPrint({
    #   opcoes_usuario$escolha_usuario
    # })



    # output$info_user <-  renderText({
    #   texto_cabecalho()
    # })

    # output$data1 <-  renderText({
    #   input$data1
    # })

    output$data1 <-  renderText({
      paste("gasugas",format(input$data2, "%m/%Y"))
    })

    output$data1class <-  renderText({
      class(input$data2)
    })



  })
}

## To be copied in the UI
# mod_testando_ui("testando_1")

## To be copied in the server
# mod_testando_server("testando_1")
