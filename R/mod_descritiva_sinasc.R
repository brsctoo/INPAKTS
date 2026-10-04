#' descritiva_sinasc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_descritiva_sinasc_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      ## Opções para o usuário selecionar -------
      fluidRow(
        column(5,
               p(),
               ### Campo para selecionar o nível geográfico escolhido -------
               selectInput(inputId = ns("data_selecionada"),
                           label = "Selecione uma das datas de intervenção", choices = " ")),
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
                      shinycssloaders::withSpinner(plotOutput(ns("pre_natal")))),
               column(6,
                      shinycssloaders::withSpinner(plotOutput(ns("cesarias"))))),
      fluidRow(column(6,
                      shinycssloaders::withSpinner(plotOutput(ns("trimestre_gestacao")))),
               column(6,
                      shinycssloaders::withSpinner(plotOutput(ns("tipo_parto"))))),
      fluidRow(column(6,
                      shinycssloaders::withSpinner(plotOutput(ns("ocorrencia_cesarea")))),
               column(6,
                      shinycssloaders::withSpinner(plotOutput(ns("semana_gestacional")))))
    ) )
}

#' descritiva_sinasc Server Functions
#'
#' @noRd
mod_descritiva_sinasc_server <- function(id, opcoes_usuario){
  moduleServer(id, function(input, output, session){
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

    # O que o usuário escolheu, no momento em que clica em "Gerar gráficos".
    escolha <- escolha_descritivo(input, opcoes_usuario)

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

    # Gráficos (ver R/utils_descritivo.R)
    graficos_descritivo(
      graficos = lista_graficos_desc(
        grafico_desc("pre_natal", "consulta_prenatal", "Número de consultas de pré-natal", topo = TRUE, cache = FALSE),
        grafico_desc("cesarias", "parto_cesarea1", "Número de cesáreas anteriores", topo = TRUE),
        grafico_desc("trimestre_gestacao", "mes_gestacao_prenatal1", "Trimestre de gestação de início do pré-natal"),
        grafico_desc("tipo_parto", "tipo_parto", "Tipo de parto"),
        grafico_desc("ocorrencia_cesarea", "cesarea_anterior_parto", "Ocorrência de cesárea antes do trabalho de parto iniciar"),
        grafico_desc("semana_gestacional", "semanas_de_gestacao", "Número de semanas de gestação no nascimento")
      ),
      indicador = "SINASC",
      titulo = "SINASC (Nascidos vivos)",
      escolha = escolha,
      input = input, output = output, session = session)
  }
  )
}



## To be copied in the UI
# mod_descritiva_sinasc_ui("descritiva_sinasc_1")

## To be copied in the server
# mod_descritiva_sinasc_server("descritiva_sinasc_1")
