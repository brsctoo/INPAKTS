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
               p(),
               ### Campo para selecionar o nível geográfico escolhido -------
               selectInput(inputId = ns("data_selecionada"), label = "Selecione uma das datas de intervenção", choices = " ")),
        column(2,
               p(),
               ### Botão gerar gráficos:-------
               actionButton(inputId = ns("gerar_graficos"),label = "Gerar gráficos"),
        )),
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
mod_descritiva_sif_gestante_server <- function(id, opcoes_usuario){
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

    ## Pop-up surgirá se nenhuma data de intervenção for selecionada e usuario clicar no botao para gerar algum gráfico-------
    observeEvent( input$gerar_graficos, {
      if(length( opcoes_usuario$date_intervention[1])==0){
        shinyalert::shinyalert(
          title = "Atenção",
          text = "Você deve selecionar no mínimo uma data de intervenção na página inicial (apresentação).",
          type = "warning",
          size = "m")
      }
    })

    # Opções disponíveis ao usuário no SelectInput mudam de acordo com a seleção do nível geográfico -------
    observeEvent(opcoes_usuario$date_intervention, {
      if(sum(is.na(opcoes_usuario$date_intervention))==2){
        updateSelectInput(inputId = "data_selecionada",
                          #label = "data1",
                          choice = " ")
      }else if (sum(is.na(opcoes_usuario$date_intervention))==1) {
        updateSelectInput(inputId = "data_selecionada",
                          #choice = c(format(opcoes_usuario$date_intervention[1],format = "%b/%Y")))
                          choice = c(opcoes_usuario$date_intervention[1]))
      }else if (sum(is.na(opcoes_usuario$date_intervention))==0) {
        updateSelectInput(inputId = "data_selecionada",
                          # choice = c(format(opcoes_usuario$date_intervention[1],format = "%b/%Y") = opcoes_usuario$date_intervention[1],
                          #            format(opcoes_usuario$date_intervention[2],format = "%b/%Y") = opcoes_usuario$date_intervention[2]))
                          choice = c(opcoes_usuario$date_intervention))
      }

    }
    )

    #Alterando os dados que serão usados de acordo com as opções do usuário
    data <- eventReactive(input$gerar_graficos, {
      req(opcoes_usuario$date_intervention[1])
      dados_sif_gestante %>%
        data_prep_desc(nivel_geografico = opcoes_usuario$nivel_geografico,
                       local= opcoes_usuario$escolha_usuario,
                       intervention_date_user = input$data_selecionada,
                       data_inicio = dados_sinasc_intervencao$data_variable)
    })


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
      req(opcoes_usuario$date_intervention[1])
      ifelse(opcoes_usuario$nivel_geografico=="PR","Gráficos do Estado do Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("Gráficos da Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("Gráficos da Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("Gráficos do Município de", opcoes_usuario$escolha_usuario))))
    })

    # Título de cabeçario da página
    output$caption <- renderText({ titulo() })


    # Gerando Gráficos -------
    output$raca <- renderPlot({
      plot.col1_teste(data(),
                      CS_RACA,
                      legenda = "Raça/cor",
                      titulo = "SIFÍLIS (gestacional)",
                      posicao_legenda="top")
    })%>%
      bindCache(opcoes_usuario$nivel_geografico,
                opcoes_usuario$escolha_usuario,
                input$data_selecionada) %>%
      bindEvent(input$gerar_graficos)

    output$idade <- renderPlot({
      plot.col1_teste(data(),
                      idade_mae1,
                      legenda = "Idade materna",
                      titulo = "SIFÍLIS (gestacional)",
                      posicao_legenda="top")
    })%>%
      bindCache(opcoes_usuario$nivel_geografico,
                opcoes_usuario$escolha_usuario,
                input$data_selecionada) %>%
      bindEvent(input$gerar_graficos)

    output$classificacao_clinica<- renderPlot({
      plot.col1_teste(data(),TPEVIDENCI, legenda = "Classificação clínica", titulo ="SIFÍLIS (gestacional)")
    })%>%
      bindCache(opcoes_usuario$nivel_geografico,
                opcoes_usuario$escolha_usuario,
                input$data_selecionada) %>%
      bindEvent(input$gerar_graficos)

    output$escolaridade <- renderPlot({
      plot.col1_teste(data(),
                      CS_ESCOL_N,
                      legenda = "Escolaridade da gestante",
                      titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(opcoes_usuario$nivel_geografico,
                opcoes_usuario$escolha_usuario,
                input$data_selecionada) %>%
      bindEvent(input$gerar_graficos)

    output$non_treponemal <- renderPlot({
      plot.col1_teste(data(),
                      TPTESTE1,
                      legenda = "Resultado do teste não treponêmico no pré-natal",
                      titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(opcoes_usuario$nivel_geografico,
                opcoes_usuario$escolha_usuario,
                input$data_selecionada) %>%
      bindEvent(input$gerar_graficos)

    output$treponemal <- renderPlot({
      plot.col1_teste(data(),
                      TPCONFIRMA,
                      legenda = "Resultado do teste treponêmico no pré-natal",
                      titulo = "SIFÍLIS (gestacional)")
    })%>%
      bindCache(opcoes_usuario$nivel_geografico,
                opcoes_usuario$escolha_usuario,
                input$data_selecionada) %>%
      bindEvent(input$gerar_graficos)
  }
  )
}

## To be copied in the UI
# mod_descritiva_sif_gestante_ui("descritiva_sif_gestante_1")

## To be copied in the server
# mod_descritiva_sif_gestante_server("descritiva_sif_gestante_1")
