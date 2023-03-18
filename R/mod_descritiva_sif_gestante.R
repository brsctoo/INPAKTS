#' descritiva_sif_gestante UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_descritiva_sif_gestante_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      ## Opções para o usuário selecionar -------
      fluidRow(
        column(5,
               #p() codigo html para inserir pequeno espaço
               p(),
               #h3("Selecione o nível geográfico:"),
               ### Botões clicáveis para o user selecionar o nível geográfico:------
               shinyWidgets::radioGroupButtons(
                 inputId = ns("radio"),
                 label = "Selecione o nível geográfico:",
                 choices = c("Estado do Paraná" = "PR", "Macrorregião" = "macro", "RS" = "micro", "Município" = "municipio"),
                 selected = "PR",
                 status = "primary"
               )),
        column(5,
               p(),
               ### Campo para selecionar o nível geográfico escolhido -------
               selectInput(inputId = ns("escolha_usuario"), label = " ", choices = "PR")),
        column(2,
               p(),
               ### Botão gerar gráficos:-------
               actionButton(inputId = ns("gerar_graficos"),label = "Gerar gráficos"),
               p(),
               ### Botão com informações:-------
               actionButton(
                 inputId = ns("info"),
                 label = "Informações",
                 icon = icon("info-circle"),
               ))),
      #hr(),
      fluidRow(column(12,
                      h3(strong(textOutput(ns("caption"))), align = "center"))),
      ## Criando layout onde gráficos serão exibido -------
      fluidRow(column(6,
                      plotOutput(ns("raca"))),
               column(6,
                      plotOutput(ns("idade")))),
      fluidRow(column(6,
                       plotOutput(ns("classificacao_clinica"))),
                column(6,
                       plotOutput(ns("escolaridade")))),
      fluidRow(column(6,
                      plotOutput(ns("non_treponemal"))),
               column(6,
                      plotOutput(ns("treponemal"))))
    ) )
}

#' descritiva_sif_gestante Server Functions
#'
#' @noRd
mod_descritiva_sif_gestante_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ##Configurando botão com as Info: -------
    observeEvent(input$info, {
      shinyWidgets::show_alert(
        type= "info",
        width = 900,
        title = NULL,
        text = tags$span(
          tags$h3("Instruções:",style = "color: steelblue;"),
          tags$h3(tags$b("Primeiro:"), "Clique para selecionar um dos quatro níveis geográficos desejado.", align = "left"),
          tags$h3(tags$b("Segundo:"), "No campo ao lado estarão disponíveis as opções para o nível geográfico selecionado.", align = "left"),
          tags$h3(tags$b("Terceiro:"), "Clique no botão Gerar gráfico para que os gráficos sejam exibidos.",align = "left")
        ),
        html = TRUE
      )
    })

    # Atualizando as opções disponíveis ao user no SelectInput de acordo com sua seleção de nível geográfico -------
    observeEvent(input$radio, {
      if(input$radio=="PR"){
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Estado do Paraná",
                          choice = "PR")
      }else if (input$radio=="macro") {
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Macrorregião:",
                          choice = levels(dados_sif_gestante$macro))
      }else if (input$radio=="micro") {
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Regional de Saúde:",
                          choice = levels(dados_sif_gestante$micro))
      }else{
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Município:",
                          choice =levels(dados_sif_gestante$municipio))
      }

    }
    )

    # Dados se alteram de acordo com as opções selecionadas pelo user e após clicar em gerar gráfico
    data <- eventReactive(input$gerar_graficos, {
      data_prep(dados = dados_sif_gestante, nivel_geografico = input$radio, local=input$escolha_usuario)})

    # Se dataset escolhido tiver menos de 30 observações um shinyalert será enviado:
    observeEvent(input$gerar_graficos, {
      if(nrow(data())<30){
        shinyalert::shinyalert(
          title = "Banco de dados selecionado possui menos de 30 observações. Gráficos não serão gerados.", text = "Por favor, escolha novas opções.", type = "info",
          size = "m")
      }
    })

    # Título de cabeçario da página altera-se de acordo com as opções selecionadas pelo user e após clicar em gerar gráfico
    titulo <- eventReactive(input$gerar_graficos, {
      ifelse(input$radio=="PR","Gráficos do Estado do Paraná",
             ifelse(input$radio=="macro",paste("Gráficos da Macrorregião",input$escolha_usuario),
                    ifelse(input$radio=="micro",paste("Gráficos da Regional de Saúde",input$escolha_usuario),
                           paste("Gráficos do Município de", input$escolha_usuario))))
    })

    # Título de cabeçario da página
    output$caption <- renderText({ titulo() })


    # Gerando Gráficos -------
    output$raca <- renderPlot({
      plot.col1(data(),data_categorica,CS_RACA,  legenda = "Raça/cor", titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(input$radio,input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    output$idade <- renderPlot({
      plot.col1(data(),data_categorica,idade_mae1, legenda = "Idade materna", titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(input$radio,input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    output$classificacao_clinica<- renderPlot({
      plot.col1(data(),data_categorica,TPEVIDENCI, legenda = "Classificação clínica", titulo ="SIFÍLIS (gestacional)")
    })%>%
      bindCache(input$radio,input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    output$escolaridade <- renderPlot({
      plot.col1(data(),data_categorica,CS_ESCOL_N, legenda = "Escolaridade da gestante", titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(input$radio,input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    output$non_treponemal <- renderPlot({
      plot.col1(data(),data_categorica,TPTESTE1, legenda = "Resultado do teste não treponêmico no pré-natal", titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(input$radio,input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    output$treponemal <- renderPlot({
      plot.col1(data(),data_categorica,TPCONFIRMA, legenda = "Resultado do teste treponêmico no pré-natal", titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(input$radio,input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)
  }
  )
}

## To be copied in the UI
# mod_descritiva_sif_gestante_ui("descritiva_sif_gestante_1")

## To be copied in the server
# mod_descritiva_sif_gestante_server("descritiva_sif_gestante_1")
