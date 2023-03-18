#' inicio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inicio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(fluid = FALSE,
                  sidebarPanel(
                    width = 3,
                    p(div(tags$a(img(src = "www/governo PR.png", height = 100),
                                 href = "https://saude.es.gov.br/"))),
                    p("A plataforma é constituída de duas grandes abas:", align = "left"),
                    p(em("1. Análise descritiva",style = "color:blue;"), "na qual temos três sub-abas:", align = "left"),
                    linebreaks(0.5),
                    p(em("2. Análise de Intervenção",style = "color:blue;"), "na qual também temos três sub-abas:", align = "left"),
                    p("Mais infromações estão no seguinte link:", tags$a(
                      href = "https://saude.es.gov.br", "Nome do Link.",
                      class = "externallink"
                    )),
                    # fluidRow(
                    #   bs4Dash::infoBox(title = "Número de casos na semana", 36, color = "danger", icon = icon("credit-card")),
                    #   bs4Dash::infoBox(title = "Número de casos no mês", 2, color = "warning", icon = icon("credit-card")),
                    #   bs4Dash::infoBox(title = "Número de casos no ano", 2, color = "maroon", icon = icon("fas fa-chart-bar"))
                    # )
                    shinyWidgets::useBs4Dash(),


                    # Opções para o usuário selecionar -------
                    p( strong("1° Passo:"), "selecione qual nível geográfico pretende realizar a análise  de intervenção (Para todo Paraná, para alguma macrorregião,
        para alguma microrregião ou para algum município)."),
                    fluidRow(
                      column(6,
                             #p() codigo html para inserir pequeno espaço
                             #h3("Selecione o nível geográfico:"),
                             ## Botões para o user selecionar o nível geográfico:------
                             selectInput(
                               inputId = ns("radio"),
                               label = "",
                               choices = c("Estado do Paraná" = "PR",
                                           "Macrorregião"     = "macro",
                                           "RS"               = "micro",
                                           "Município"        = "municipio"),
                               selected = "PR"
                             ))),
                    # shinyWidgets::radioGroupButtons(
                    #   inputId = ns("radio"),
                    #   label = "1° Passo",
                    #   choices = c("Estado do Paraná" = "PR",
                    #               "Macrorregião"     = "macro",
                    #               "RS"               = "micro",
                    #               "Município"        = "municipio"),
                    #   selected = "PR",
                    #   status = "primary"
                    # ))),
                    p( strong("2° Passo:"), "agora, selecione a opção de acordo com nível geográfico escolhido (ex:
        se você selecionou munícípio na etapa anterior, então você deverá escolher o município desejado
         (Curitiba, maringá, etc)."),
                    fluidRow(
                      column(6,
                             #p(),
                             ## Campo para selecionar o nível geográfico escolhido -------
                             selectInput(inputId = ns("escolha_usuario"),
                                         label = " ", choices = "PR"))),
                    p( strong("3° Passo:"), "Por fim, selecione até duas datas de intervenção."),
                    fluidRow(
                      column(6,
                             #p(),
                             ## Campo para selecionar a data da intervenção 1 -------
                             shinyWidgets::airDatepickerInput(inputId = ns("date_intervention"),
                                                              label = "",
                                                              language = 'pt-BR',
                                                              # value = c("2019-02-01","2020-10-01"),
                                                              width = "250px",
                                                              maxDate = as.Date(max(dados_sinasc_intervencao$data_variable))-months(5),
                                                              minDate = min(as.Date(dados_sinasc_intervencao$data_variable)),
                                                              view = "months", #editing what the popup calendar shows when it opens
                                                              minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                                              dateFormat = "MM/yyyy",
                                                              clearButton=T,
                                                              range = T)))
                  ),
                  mainPanel(
                    width = 9,

                    #tags$b("...") torna o texto em negrito
                    #p(...) delimita um parágrafo
                    p(div(tags$a(img(src = "www/governo PR.png", height = 100),
                                 href = "https://saude.es.gov.br/")), align = "center"),
                    p("A plataforma é constituída de duas grandes abas:", align = "left"),
                    p(em("1. Análise descritiva",style = "color:blue;"), "na qual temos três sub-abas:", align = "left"),
                    linebreaks(0.5),
                    p(em("2. Análise de Intervenção",style = "color:blue;"), "na qual também temos três sub-abas:", align = "left"),
                    p("Mais infromações estão no seguinte link:", tags$a(
                      href = "https://saude.es.gov.br", "Nome do Link.",
                      class = "externallink"
                    ))
                  )
    )
  )
}

#' inicio Server Functions
#'
#' @noRd
mod_inicio_server <- function(id, opcoes_usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Opções disponíveis ao usuário no SelectInput mudam de acordo com a seleção do nível geográfico -------
    observeEvent(input$radio, {
      ## Se nível geográfico todo PR (input$radio=="PR"):
      if(input$radio=="PR"){
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Estado do Paraná",
                          choice = "PR")
        ## Se nível geográfico for macrorregião (input$radio=="macro"):
      }else if (input$radio=="macro") {
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Macrorregião:",
                          choice = levels(dados_sinasc_intervencao$macro))
        ## Se nível geográfico for microrregião (input$radio=="micro"):
      }else if (input$radio=="micro") {
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Regional de Saúde:",
                          choice = levels(dados_sinasc_intervencao$micro))
        ## Ou se for município (input$radio=="municipio"):
      }else{
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Município:",
                          choice =levels(dados_sinasc_intervencao$municipio))
      }

    }
    )

    # toListen <- reactive({
    #   list(input$radio, input$escolha_usuario,input$date_intervention)
    # })

    # Pop-up com as instruções para o usuário ----------
    # observeEvent(input$id$inicio, {
    #   shinyWidgets::show_alert(
    #     type= "info",
    #     width = 1000,
    #     size = "lg",
    #     title = "Na aba Apresentação faça o seguinte:",
    #     text = tags$span(
    #       #tags$h3("Instruções:",style = "color: steelblue;"),
    #       tags$h5(tags$b("1° Passo:",style = "color: steelblue;"), "Selecione um dos quatro níveis geográficos desejado.", align = "left"),
    #       tags$h5(tags$b("2° Passo:"), "Selecione umas das opções para o nível geográfico selecionado.", align = "left"),
    #       tags$h5(tags$b("3° Passo:"), "Selecione no mínimo uma data de intervenção", align = "left"),
    #       tags$h5("Resultados obtidos por todo site estarão de acordo com estas configurações.", align = "left")
    #
    #     ),
    #     html = TRUE
    #   )
    # })


    # Armazenando as opções selecionadas pelo usuário para usar em outros módulos ------

    ## opcoes_usuario$date_intervention armazenará as data de intervenção selecionadas-----
    observeEvent( input$date_intervention, {
      opcoes_usuario$date_intervention <- c(input$date_intervention[1],input$date_intervention[2])
    })

    ## opcoes_usuario$nivel_geografico armazenará o nível geo selecionado (PR, Macro, etc)-----

    observeEvent( input$radio , {
      opcoes_usuario$nivel_geografico <- input$radio
      #  opcoes_usuario$nivel_geografico_nome armazenará o nível geo selecionado nome por extenso apenas para usar nos títulos
      # nos outros módulos
      opcoes_usuario$nivel_geografico_nome <-  ifelse(input$radio=="PR","Estado do Paraná",
                                                      ifelse(input$radio=="macro", "Macrorregião",
                                                             ifelse(input$radio=="micro", "Regional de saúde",opcoes_usuario$nivel_geografico)))
    })

    ## input$escolha_usuario armazenará opção escolhida pelo usuario para um dado nivel geográfico (regional de saude 1, leste, maringa etc..)-----

    observeEvent( input$escolha_usuario , {
      opcoes_usuario$escolha_usuario <- input$escolha_usuario
    })
  })

}

## To be copied in the UI
# mod_inicio_ui("inicio_ui_1")

## To be copied in the server
# mod_inicio_server("inicio_ui_1")
