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


    # Mapa sinasc --------------------------------------------------------------

    titulo_sinasc <- eventReactive(input$gerar_graficos, {
      ifelse(
        input$radio == "PR",
        "Nascidos Vivos (SINASC) do Estado do Paraná",
        paste0(
          "Nascidos Vivos (SINASC) da ",
          input$escolha_usuario,
          "ª Regional de Saúde"
        )
      )
    })

    output$titulo_sinasc <- renderText(titulo_sinasc())


    mapa_sinasc <- eventReactive(input$gerar_graficos, {
      req(input$data_selecionada)
      if(input$radio == "RS"){
        RSmapOrd(
          varToPlot =  dados_map_sinasc[,as.character(input$data_selecionada)],
          legeName = "Mudança na tendência",
          mun = dados_map_sinasc$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sinasc[,as.character(input$data_selecionada)])
        )}
      else{
        ggpubr::ggarrange(
          UFmapOrd(
            varToPlot = dados_map_sinasc[,as.character(input$data_selecionada)],
            mun = dados_map_sinasc$municipio,
            legeName = "Mudança na tendência por munípio no PR",
            legeLabels = levels(dados_map_sinasc[,as.character(input$data_selecionada)])
          )$map,
          UFmapOrd(
            varToPlot = dados_map_sinasc_rs[,as.character(input$data_selecionada)],
            mun = dados_map_sinasc_rs$municipio,
            legeName = "Mudança na tendência no PR por RS",
            legeLabels = levels(dados_map_sinasc_rs[,as.character(input$data_selecionada)])
          )$map)
          #common.legend = T)
      }
    })

    output$mapa_sinasc <- renderPlot({
      mapa_sinasc()
    })%>%
      bindCache(input$data_selecionada,
                input$radio,
                input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)


    # Mapa sim neonatal--------------------------------------------------------

    titulo_sim_neonatal <- eventReactive(input$gerar_graficos, {
      ifelse(
        input$radio == "PR",
        "Óbitos neonatais (SIM) do Estado do Paraná",
        paste0(
          "Óbitos neonatais (SIM) da ",
          input$escolha_usuario,
          "ª Regional de Saúde"
        )
      )
    })

    output$titulo_sim_neonatal <- renderText(titulo_sim_neonatal())


    mapa_sim_neonatal <- eventReactive(input$gerar_graficos, {
      req(input$data_selecionada)
      if(input$radio == "RS"){
        #Plota o mapa da Regional de Saúde
        RSmapOrd(
          varToPlot = dados_map_sim_neonatal[,as.character(input$data_selecionada)],
          legeName = "Mudança na tendência",
          mun = dados_map_sim_neonatal$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sim_neonatal[,as.character(input$data_selecionada)])
        )}
      else{
        #Plota o mapa do estado do PR por município ao lado do mapa do PR por RS
        ggpubr::ggarrange(
          UFmapOrd(
            varToPlot = dados_map_sim_neonatal[,as.character(input$data_selecionada)],
            mun = dados_map_sim_neonatal$municipio,
            legeName = "Mudança na tendência por munípio no PR",
            legeLabels = levels(dados_map_sim_neonatal[,as.character(input$data_selecionada)])
          )$map,
          UFmapOrd(
            varToPlot = dados_map_sim_neonatal_rs[,as.character(input$data_selecionada)],
            mun = dados_map_sim_neonatal_rs$municipio,
            legeName = "Mudança na tendência por RS no PR",
            legeLabels = levels(dados_map_sim_neonatal_rs[,as.character(input$data_selecionada)])
          )$map)
      }
    })

    output$mapa_sim_neonatal <- renderPlot({
      mapa_sim_neonatal()
    })%>%
      bindCache(input$data_selecionada,
                input$radio,
                input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    # Mapa sim materno--------------------------------------

    titulo_sim_materno <- eventReactive(input$gerar_graficos, {
      ifelse(
        input$radio == "PR",
        "Óbitos maternos (SIM) do Estado do Paraná",
        paste0(
          "Óbitos maternos (SIM) da ",
          input$escolha_usuario,
          "ª Regional de Saúde"
        )
      )
    })

    output$titulo_sim_materno <- renderText(titulo_sim_materno())

    mapa_sim_materno <- eventReactive(input$gerar_graficos, {
      req(input$data_selecionada)
      if(input$radio == "RS"){
        #Plota o mapa da Regional de Saúde
        RSmapOrd(
          varToPlot = dados_map_sim_materno[,as.character(input$data_selecionada)],
          legeName = "Mudança na tendência",
          mun = dados_map_sim_materno$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sim_materno[,as.character(input$data_selecionada)])
        )}
      else{
        #Plota o mapa do estado do PR por município ao lado do mapa do PR por RS
        ggpubr::ggarrange(
          UFmapOrd(
            varToPlot = dados_map_sim_materno[,as.character(input$data_selecionada)],
            mun = dados_map_sim_materno$municipio,
            legeName = "Mudança na tendência por munípio no PR",
            legeLabels = levels(dados_map_sim_materno[,as.character(input$data_selecionada)])
          )$map,
          UFmapOrd(
            varToPlot = dados_map_sim_materno_rs[,as.character(input$data_selecionada)],
            mun = dados_map_sim_materno_rs$municipio,
            legeName = "Mudança na tendência por RS no PR",
            legeLabels = levels(dados_map_sim_materno_rs[,as.character(input$data_selecionada)])
          )$map)
      }
    })

    output$mapa_sim_materno <- renderPlot({
      mapa_sim_materno()
    })%>%
      bindCache(input$data_selecionada,
                input$radio,
                input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    # Mapa sif gestante--------------------------------------------------------

    titulo_sif_gestante <- eventReactive(input$gerar_graficos, {
      ifelse(
        input$radio == "PR",
        "Casos de sífilis gestacional do Estado do Paraná",
        paste0(
          "Casos de sífilis gestacional da ",
          input$escolha_usuario,
          "ª Regional de Saúde"
        )
      )
    })

    output$titulo_sif_gestante <- renderText(titulo_sif_gestante())

    mapa_sif_gestante <- eventReactive(input$gerar_graficos, {
      req(input$data_selecionada)
      if(input$radio == "RS"){
        #Plota o mapa da Regional de Saúde
        RSmapOrd(
          varToPlot = dados_map_sif_gestante[,as.character(input$data_selecionada)],
          legeName = "Mudança na tendência",
          mun = dados_map_sif_gestante$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sif_gestante[,as.character(input$data_selecionada)])
        )}
      else{
        #Plota o mapa do estado do PR por município ao lado do mapa do PR por RS
        ggpubr::ggarrange(
          UFmapOrd(
            varToPlot = dados_map_sif_gestante[,as.character(input$data_selecionada)],
            mun = dados_map_sif_gestante$municipio,
            legeName = "Mudança na tendência por munípio no PR",
            legeLabels = levels(dados_map_sif_gestante[,as.character(input$data_selecionada)])
          )$map,
          UFmapOrd(
            varToPlot = dados_map_sif_gestante_rs[,as.character(input$data_selecionada)],
            mun = dados_map_sif_gestante_rs$municipio,
            legeName = "Mudança na tendência por RS no PR",
            legeLabels = levels(dados_map_sif_gestante_rs[,as.character(input$data_selecionada)])
          )$map)
      }
    })


    output$mapa_sif_gestante <- renderPlot({
      mapa_sif_gestante()
    })%>%
      bindCache(input$data_selecionada,
                input$radio,
                input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    # Mapa sif congenita--------------------------------------------------------

    titulo_sif_congenita <- eventReactive(input$gerar_graficos, {
      ifelse(
        input$radio == "PR",
        "Casos de sífilis congenita do Estado do Paraná",
        paste0(
          "Casos de sífilis congenita da ",
          input$escolha_usuario,
          "ª Regional de Saúde"
        )
      )
    })

    output$titulo_sif_congenita <- renderText(titulo_sif_congenita())

    mapa_sif_congenita <- eventReactive(input$gerar_graficos, {
      if(input$radio == "RS"){
        #Plota o mapa da Regional de Saúde
        RSmapOrd(
          varToPlot = dados_map_sif_congenita[,as.character(input$data_selecionada)],
          legeName = "Mudança na tendência",
          mun = dados_map_sif_congenita$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sif_congenita[,as.character(input$data_selecionada)])
        )}
      else{
        #Plota o mapa do estado do PR por município ao lado do mapa do PR por RS
        ggpubr::ggarrange(
          UFmapOrd(
            varToPlot = dados_map_sif_congenita[,as.character(input$data_selecionada)],
            mun = dados_map_sif_congenita$municipio,
            legeName = "Mudança na tendência por munípio no PR",
            legeLabels = levels(dados_map_sif_congenita[,as.character(input$data_selecionada)])
          )$map,
          UFmapOrd(
            varToPlot = dados_map_sif_congenita_rs[,as.character(input$data_selecionada)],
            mun = dados_map_sif_congenita_rs$municipio,
            legeName = "Mudança na tendência por RS no PR",
            legeLabels = levels(dados_map_sif_congenita_rs[,as.character(input$data_selecionada)])
          )$map)
      }
    })

    output$mapa_sif_congenita <- renderPlot({
      mapa_sif_congenita()
    })%>%
      bindCache(input$data_selecionada,
                input$radio,
                input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    })
  }

  ## To be copied in the UI
  # mod_analise_geografica_ui("analise_geografica_1")

  ## To be copied in the server
  # mod_analise_geografica_server("analise_geografica_1")
