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
          5,
          #p() codigo html para inserir pequeno espaço
          p(),

          ### Botões clicáveis para o user selecionar o nível geográfico:------
          shinyWidgets::radioGroupButtons(
            inputId = ns("radio"),
            label = "Selecione o nível geográfico:",
            choices = c("Estado do Paraná" = "PR", "RS" = "RS"),
            selected = "PR",
            status = "primary"
          )
        ),
        column(4,
               p(),
               ### Campo para selecionar o nível geográfico escolhido -------
               selectInput(
                 inputId = ns("escolha_usuario"),
                 label = " ",
                 choices = "PR"
               )),
        column(
          3,
          p(),
          ### Botão gerar gráficos:-------
          actionButton(inputId = ns("gerar_graficos"), label = "Gerar Mapas"),
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
      ## Criando layout onde o título os dois gráficos serão exibidos na ui -------
      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns("titulo_sinasc"))),  align = "center"
                      ))),
      fluidRow(column(12,
                      plotOutput(
                        ns("mapa_sinasc"), height = 500
                      ))),

      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns(
                          "titulo_sim_neonatal"
                        ))), align = "center"
                      ))),
      fluidRow(column(12,
                      plotOutput(
                        ns("mapa_sim_neonatal"), height = 500
                      ))),

      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns(
                          "titulo_sim_materno"
                        ))), align = "center"
                      ))),
      fluidRow(column(12,
                      plotOutput(
                        ns("mapa_sim_materno"), height = 500
                      ))),

      fluidRow(column(12,
                      h3(
                        strong(textOutput(ns(
                          "titulo_sif_gestante"
                        ))), align = "center"
                      ))),
      fluidRow(column(12,
                      plotOutput(
                        ns("mapa_sif_gestante"), height = 500
                      ))),
    )
  )
}

#' analise_geografica Server Functions
#'
#' @noRd
mod_analise_geografica_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ##Configurando botão com as Info: -------
    observeEvent(input$info, {
      shinyWidgets::show_alert(
        type = "info",
        width = 900,
        title = NULL,
        text = tags$span(
          tags$h3("Instruções:", style = "color: steelblue;"),
          tags$h3(
            tags$b("Primeiro:"),
            "Clique para selecionar se você quer informações para todo estado do PR ou apenas para uma RS",
            align = "left"
          ),
          tags$h3(
            tags$b("Segundo:"),
            "Caso selecione RS, no campo ao lado estarão listadas as opções para as Regionais de Saúde existentes.",
            align = "left"
          ),
          tags$h3(
            tags$b("Quarto:"),
            "Clique no botão Gerar Mapas para que os mapas sejam exibidos.",
            align = "left"
          )
        ),
        html = TRUE
      )
    })


    # Opções disponíveis ao usuário no SelectInput mudam de acordo com a seleção do nível geográfico -------
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



    #Mapa sinasc

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
      ifelse(
        input$radio == "RS",
        RSmapOrd(
          varToPlot = dados_map_sinasc$trendChangeCat,
          legeName = "Mudança na tendência",
          mun = dados_map_sinasc$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sinasc$trendChangeCat)
        ),
        UFmapOrd(
          varToPlot = dados_map_sinasc$trendChangeCat,
          mun = dados_map_sinasc$municipio,
          legeName = "Mudança na tendência",
          legeLabels = levels(dados_map_sinasc$trendChangeCat)
        )
      )
    })

    output$mapa_sinasc <- renderPlot({
      mapa_sinasc()
    })%>%
      bindCache(input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    #Mapa sim neonatal

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
      ifelse(
        input$radio == "RS",
        RSmapOrd(
          varToPlot = dados_map_sim_neonatal$trendChangeCat,
          legeName = "Mudança na tendência",
          mun = dados_map_sim_neonatal$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sim_neonatal$trendChangeCat)
        ),
        UFmapOrd(
          varToPlot = dados_map_sim_neonatal$trendChangeCat,
          mun = dados_map_sim_neonatal$municipio,
          legeName = "Mudança na tendência",
          legeLabels = levels(dados_map_sim_neonatal$trendChangeCat)
        )
      )
    })

    output$mapa_sim_neonatal <- renderPlot({
      mapa_sim_neonatal()
    })%>%
      bindCache(input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    #Mapa sim materno

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
      ifelse(
        input$radio == "RS",
        RSmapOrd(
          varToPlot = dados_map_sim_materno$trendChangeCat,
          legeName = "Mudança na tendência",
          mun = dados_map_sim_materno$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sim_materno$trendChangeCat)
        ),
        UFmapOrd(
          varToPlot = dados_map_sim_materno$trendChangeCat,
          mun = dados_map_sim_materno$municipio,
          legeName = "Mudança na tendência",
          legeLabels = levels(dados_map_sim_materno$trendChangeCat)
        )
      )
    })

    output$mapa_sim_materno <- renderPlot({
      mapa_sim_materno()
    })%>%
      bindCache(input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)

    #Mapa sif gestante

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
      ifelse(
        input$radio == "RS",
        RSmapOrd(
          varToPlot = dados_map_sif_gestante$trendChangeCat,
          legeName = "Mudança na tendência",
          mun = dados_map_sif_gestante$municipio,
          RS =  input$escolha_usuario,
          legeLabels = levels(dados_map_sif_gestante$trendChangeCat)
        ),
        UFmapOrd(
          varToPlot = dados_map_sif_gestante$trendChangeCat,
          mun = dados_map_sif_gestante$municipio,
          legeName = "Mudança na tendência",
          legeLabels = levels(dados_map_sif_gestante$trendChangeCat)
        )
      )
    })

    output$mapa_sif_gestante <- renderPlot({
      mapa_sif_gestante()
    })%>%
      bindCache(input$escolha_usuario) %>%
      bindEvent(input$gerar_graficos)


  })
}

## To be copied in the UI
# mod_analise_geografica_ui("analise_geografica_1")

## To be copied in the server
# mod_analise_geografica_server("analise_geografica_1")
