#' analise_geografica UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analise_geografica_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## Opções para o usuário selecionar -------
      fluidRow(
        column(
          3,
          #p() codigo html para inserir pequeno espaço
          p(),

          ### Botões clicáveis para o usuário selecionar o nível geográfico:------
          shinyWidgets::radioGroupButtons(
            inputId = ns("radio"),
            label = "Selecione o nível geográfico:",
            choices = c("Estado do Paraná" = "PR",
                        "RS" = "RS"),
            selected = "PR",
            status = "primary"
          )
        ),
        column(2,
               p(),
               ### Campo para o usuário selecionar o estado ou uma das Regionais de saúde -------
               selectInput(
                 inputId = ns("escolha_usuario"),
                 label = " ",
                 choices = "PR"
               )),

        column(4,
               p(),
               ### Campo para o usuário selecionar umas das datas de intervenção da pag. inicial -------
               selectInput(inputId = ns("data_selecionada"),
                           label = "Selecione uma das datas de intervenção",
                           choices = " ")),

        column(
          3,
          p(),
          ### Botão gerar gráficos:-------
          actionButton(inputId = ns("gerar_graficos"),
                       label = "Gerar Mapas"),
          p(),
          ### Botão com informações:-------
          actionButton(
            inputId = ns("info"),
            label = "Informações",
            icon = icon("info-circle")
          )
        )
      ),
      hr(),
      ## Criando layout onde o título e os gráficos serão exibidos na ui -------
      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns("titulo_sinasc"))),  align = "center"
                      ))),

      fluidRow(column(12,
                      shinycssloaders::withSpinner(
                        plotOutput(ns("mapa_sinasc"), height = 500)
                      )
      )),

      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns(
                          "titulo_sim_neonatal"
                        ))), align = "center"
                      ))),
      fluidRow(column(12,
                      shinycssloaders::withSpinner(
                        plotOutput(ns("mapa_sim_neonatal"), height = 500)
                      )
      )),

      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns(
                          "titulo_sim_materno"
                        ))), align = "center"
                      ))),
      fluidRow(column(12,
                    shinycssloaders::withSpinner(
                      plotOutput(ns("mapa_sim_materno"), height = 500)
                    )
      )),
      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns(
                          "titulo_sif_gestante"
                        ))), align = "center"
                      ))),
      fluidRow(column(12,
                    shinycssloaders::withSpinner(
                      plotOutput(ns("mapa_sif_gestante"), height = 500)
                    )
      )),
      fluidRow(column(12,
                      h3(
                        strong(
                          textOutput(ns("titulo_sif_congenita"))),
                        align = "center"
                      ))),
      fluidRow(column(12,
                        shinycssloaders::withSpinner(
                          plotOutput(ns("mapa_sif_congenita"), height = 500)
                        )
      )),
    )
  )
}

#' analise_geografica Server Functions
#'
#' @noRd
mod_analise_geografica_server <- function(id, opcoes_usuario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ##Configurando botão com as Info: -------
    observeEvent(input$info, {
      shinyWidgets::show_alert(
        type = "info",
        width = 1000,
        title = NULL,
        text = tags$span(
          tags$h3("Instruções:", style = "color: steelblue;"),
          tags$h2(
            tags$b("Primeiro:"),
            "Clique para selecionar se você quer informações para todo estado do PR ou apenas para uma RS",
            align = "left"
          ),
          tags$h2(
            tags$b("Segundo:"),
            "Caso selecione RS, no campo ao lado estarão listadas as opções para as Regionais de Saúde existentes.",
            align = "left"
          ),
          tags$h2(
            tags$b("Quarto:"),
            "Clique no botão Gerar Mapas para que os mapas sejam exibidos.",
            align = "left"
          ),
          tags$h2(
            tags$b("OBS:"),
            "as cores de cada região no mapa, de acordo com a legenda à direita da figura, indicam se houve uma mudança estatisticamente significante na tendência observada",
            align = "left"
          )
        ),
        html = TRUE
      )
    })

    # Pop-ups -------

    ## Pop-up surgirá se nenhuma data de intervenção for selecionada e usuario clicar no botao Gerar mapa-------
    observeEvent( input$gerar_graficos, {
      if(length( opcoes_usuario$date_intervention[1])==0){
        shinyalert::shinyalert(
          title = "Atenção",
          text = "Você deve selecionar no mínimo uma data de intervenção na página inicial (apresentação).",
          type = "warning",
          size = "m")
      }
    })

    # Opções disponíveis ao usuário no SelectInput mudam de acordo com as datas de intervnção escolhidas na pág. iniciail -------
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


    observeEvent(input$radio, {
      ## Se nível geográfico todo PR (input$radio=="PR"):
      if (input$radio == "PR") {
        updateSelectInput(inputId = "escolha_usuario",
                          label = "Estado do Paraná",
                          choice = "PR")
        ## Se nível geográfico for macrorregião (input$radio=="macro"):
      } else{
        updateSelectInput(
          inputId = "escolha_usuario",
          label = "Regional de Saúde:",
          choice = as.character(levels(
            as.factor(dados_map_sinasc$micro)
          ))
        )

      }
    })


    # Mapas

    # O que o usuário escolheu, no momento em que clica em "Gerar Mapas"
    escolha <- escolha_mapa(input)

    # Títulos, fila, cache e mapas (ver utils_mapas.R). A ordem é a ordem na tela.
    mapas_geograficos(
      lista_mapas_geo(
        mapa_geo("sinasc", "Nascidos Vivos (SINASC)", dados_map_sinasc, dados_map_sinasc_rs),
        mapa_geo("sim_neonatal", "Óbitos neonatais (SIM)", dados_map_sim_neonatal, dados_map_sim_neonatal_rs),
        mapa_geo("sim_materno", "Óbitos maternos (SIM)", dados_map_sim_materno, dados_map_sim_materno_rs),
        mapa_geo("sif_gestante", "Casos de sífilis gestacional", dados_map_sif_gestante, dados_map_sif_gestante_rs),
        mapa_geo("sif_congenita", "Casos de sífilis congenita", dados_map_sif_congenita, dados_map_sif_congenita_rs)
      ),
      escolha, input, output, session
    )

    })
  }

  ## To be copied in the UI
  # mod_analise_geografica_ui("analise_geografica_1")

  ## To be copied in the server
  # mod_analise_geografica_server("analise_geografica_1")
