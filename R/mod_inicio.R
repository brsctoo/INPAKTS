#' inicio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd


#' @importFrom shiny NS tagList
mod_inicio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      fluid = FALSE,
      sidebarPanel(
        style  = "margin-right: 0px;",
        fluid = FALSE,
        width = 3,
        p(
          HTML(
            '<center><img src="www/logo_i_sem_pingo.png" height="90" width="75%"></center>'
          )
        ),

        useBs4Dash(),
        br(),
        br(),
        br(),

        # Opções para o usuário selecionar -------
        p("DEFINIÇÕES para todas as análises nesta plataforma:", style = "font-size: 18px"),
        br(),
        p("1)	Selecione:", style = "font-size: 18px"),
        selectInput(
          inputId = ns("radio"),
          label = "Nível Geográfico:",
          choices = c(
            "Estado do Paraná" = "PR",
            "Macrorregião"     = "macro",
            "RS"               = "micro",
            "Município"        = "municipio"
          ),
          selected = "PR"
        ),
        br(),
        br(),
        ## Campo para selecionar o nível geográfico escolhido -------
        selectInput(
          inputId = ns("escolha_usuario"),
          label = "Localidade:",
          choices = "PR"
        ),
        br(),
        br(),
        ## Campo para selecionar a data da intervenção 1 -------
        shinyWidgets::airDatepickerInput(
          inputId = ns("date_intervention"),
          label = "Até 2 datas de intervenção: ",
          language = 'pt-BR',
          # value = c("2019-02-01","2020-10-01"),
          width = "250px",
          maxDate = as.Date(max(
            dados_sinasc_intervencao$data_variable
          )) - months(5),
          minDate = "2015-05-01",
          #minDate = min(as.Date(
          #  dados_sinasc_intervencao$data_variable
         # )),
          view = "months",
          #editing what the popup calendar shows when it opens
          minView = "months",
          #making it not possible to go down to a "days" view and pick the wrong date
          dateFormat = "MM/yyyy",
          clearButton = T,
          range = T
        ),
        br(),
        br(),
        br(),
        p(
          "2)	Para cada uma das abas do MENU no topo da página, escolha um dos BANCOS DE DADOS para o qual objetiva gerar as análises.",
          style = "font-size: 18px"
        ),
        br()

      ),
      sidebarPanel(
        style = "background-color: #6baed630;",
        height = 15,

        width = 9,
        fluid = FALSE,

        #tags$b("...") torna o texto em negrito
        #p(...) delimita um parágrafo
        br(),
        p(
          tags$b(
            "Plataforma de gestão e monitoramento do impacto de intervenções e eventos
            externos em Séries Temporais na saúde materno-infantil, da mulher e da criança"
          ),
          align = "center",
          style = "font-size: 30px"
        ),
        br(),
        h5(
          "Você gostaria de visualizar e monitorar o impacto de eventos externos,
          como políticas públicas (por exemplo, o Programa Rede Mãe Paranaense) e
          ocorrências de agravos (como a COVID-19), na saúde materno-infantil, levando
          em conta fatores de risco associados, como idade, raça e gênero? Aqui, você
          pode realizar análises de séries temporais de forma adequada!"
        ),
        br(),
        h5("O que você precisa ter em mãos antes de iniciar:"),
        p(
          "Uma ou duas datas (mês/ano) da(s) intervenção(ões) que deseja analisar."
        ),
        br(),
        h5("O que a INPAKTS disponibiliza?"),
        p(
          "Modelos estatísticos adaptativos para os dados e localidade escolhida de
          modo que você poderá saber qual a mudança em tendência ocorrida após cada
          intervenção. E melhor, essa tendência está em porcentagem de mudança mensal.
          Você também poderá visualizar no mapa do estado do Paraná, quais municípios
          apresentaram tendência de aumento ou redução após a intervenção analisada."
        ),
        br(),
        h5(
          "Por exemplo, no que se refere à pandemia, o impacto da COVID-19 pode
          ser estimado no risco de vulnerabilidade para:"
        ),
        tags$ul(
          tags$li("mortalidade neonatal (geral, precoce e tardia);"),
          tags$li("mortalidade materna;"),
          tags$li("Sífilis (gestacional)"),
          tags$li("na transmissão vertical ou perinatal da Sífilis (congênita)")
        ),
        br(),
        h5("como também para:"),
        tags$ul(
          tags$li(
            "acesso aos cuidados durante o pré-natal e após o parto, na saúde gestacional,
            nas taxas relacionadas ao parto, nascimento prematuro e outros fatores que podem
            impactar desfechos da gravidez e do desenvolvimento infantil."
          )
        ),
        br(),
        br(),
        br(),
        br(),
        p(tags$a(img(src = "www/database-solid.png", height = 30,width = 25, style= "margin-right: 5px;")), " ", strong("A base de dados contém informações de ",paste0(
          format(as.Date(min(dados_sinasc_intervencao$data_variable)), "%B de %Y"), " até ",
          format(as.Date(max(dados_sinasc_intervencao$data_variable)), "%B de %Y"))
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
                          choice =levels(municipios_PR$municipio))
                          #choice =levels(dados_sinasc_intervencao$municipio))
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


    observeEvent(input$escolha_usuario , {
      opcoes_usuario$escolha_usuario <- input$escolha_usuario
    })

    #datas de inicio e fim do banco de dados (dados_sinasc)

    inicio_fim = reactive({
      fim = dados_sinasc %>% arrange(desc(data_variable)) %>% select(data_variable) %>% mutate(data_variable = as.Date(data_variable))
      fim = max(fim$data_variable)

      inicio = dados_sinasc %>% arrange(desc(data_variable)) %>% select(data_variable) %>% mutate(data_variable = as.Date(data_variable))
      inicio = min(inicio$data_variable)

      paste0("A base de dados contém informações de ", format(inicio, "%b de %Y"), " até ", format(fim, "%b de %Y"))
      })

    output$info_text <- renderText({
      inicio_fim()
    })

  })




}

## To be copied in the UI
# mod_inicio_ui("inicio_ui_1")

## To be copied in the server
# mod_inicio_server("inicio_ui_1")
