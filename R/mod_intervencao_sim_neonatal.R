#' intervencao_sim_neonatal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intervencao_sim_neonatal_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidPage(

      #Para gerar os pop-ups:

      #shinyWidgets::useSweetAlert(),

      shinyWidgets::useBs4Dash(),


      # Criando layout onde gráficos e a tabelas serão exibidos -------
      bs4Dash::bs4Card(
        title = textOutput(ns("info_user_geral")),#htmlOutput(ns("info_user")),
        status = "primary",
        width = 12,
        collapsed = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(8),
          column(2,
                 actionButton(inputId = ns("gerar_graficos"),
                              label = "Gerar resultados",
                              width = "140px")),
          column(2,
                 shinyjs::hidden(downloadButton(ns("relatorio"),
                                                label = "Análise de resíduos")))),
        fluidRow(
          column(2,
                 tableOutput((ns("info_modelo_ajustado")))),
          column(10,
                 plotly::plotlyOutput(ns("plot_geral"))))),
      hr(),
      bs4Dash::bs4Card(
        title = textOutput(ns("info_user_sexo")),#htmlOutput(ns("info_user")),
        status = "primary",
        width = 12,
        collapsed = TRUE,
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(10),
          column(2,
                 actionButton(inputId = ns("gerar_resultado_sexo"),label = "Gerar resultados"))),
        fluidRow(
          column(2,
                 selectInput(inputId = ns("sexo"), label = "Escolha o sexo", choices = c("Feminino","Masculino")),
                 tableOutput((ns("info_modelo_ajustado_sexo")))),
          column(10,
                 plotly::plotlyOutput(ns("plot_sexo"))))),
      hr(),
      bs4Dash::bs4Card(
        title = textOutput(ns("info_user_mortalidade")),#htmlOutput(ns("info_user")),
        status = "primary",
        width = 12,
        collapsed = TRUE,
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(10),
          column(2,
                 actionButton(inputId = ns("gerar_resultado_tipo_mortalidade"),label = "Gerar resultados"))),
        fluidRow(
          column(2,
                 selectInput(inputId = ns("tipo_mortalidade"),
                             label = "Escolha a mortalidade",
                             choices = c("Fetal" = "fetal",
                                         "Neonatal precoce" = "neonatal_precoce",
                                         "Neonatal tardia" = "neonatal_tardia")),
                 tableOutput((ns("info_modelo_ajustado_tipo_mortalidade")))),
          column(10,
                 plotly::plotlyOutput(ns("plot_idade"))))),
      hr(),
      bs4Dash::bs4Card(
        title = textOutput(ns("info_user_raca")),#htmlOutput(ns("info_user")),
        status = "primary",
        width = 12,
        collapsed = TRUE,
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(10),
          column(2,
                 actionButton(inputId = ns("gerar_resultado_raca"),label = "Gerar resultados"))),
        fluidRow(
          column(2,
                 selectInput(inputId = ns("raca"),
                             label = "Escolha a raça/cor",
                             choices = levels(dados_sim_neonatal_intervention$raca_cor)[1:2]),
                 tableOutput((ns("info_modelo_ajustado_raca")))),
          column(10,
                 plotly::plotlyOutput(ns("plot_raca")))))
    )
  )
}

#' intervencao_sim_neonatal Server Functions
#'
#' @noRd
mod_intervencao_sim_neonatal_server <- function(id, opcoes_usuario){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Pop-ups -------

    # Pop-up surgirá se nenhuma data de intervenção for selecionada e usuario clicar no botao para gerar algum gráfico
    observeEvent( input$gerar_graficos, {
      if(length( opcoes_usuario$date_intervention[1])==0){
        shinyalert::shinyalert(
          title = "Atenção",
          text = "Você deve selecionar no mínimo uma data de intervenção na página inicial (apresentação).",
          type = "warning",
          size = "m")
      }
    })

    observeEvent( input$gerar_resultado_idade, {
      if(length( opcoes_usuario$date_intervention[1])==0){
        shinyalert::shinyalert(
          title = "Atenção",
          text = "Você deve selecionar no mínimo uma data de intervenção na página inicial (apresentação).",
          type = "warning",
          size = "m")
      }
    })

    observeEvent( input$gerar_resultado_sexo, {
      if(length( opcoes_usuario$date_intervention[1])==0){
        shinyalert::shinyalert(
          title = "Atenção",
          text = "Você deve selecionar no mínimo uma data de intervenção na página inicial (apresentação).",
          type = "warning",
          size = "m")
      }
    })

    observeEvent( input$gerar_resultado_raca, {
      if(length( opcoes_usuario$date_intervention[1])==0){
        shinyalert::shinyalert(
          title = "Atenção",
          text = "Você deve selecionar no mínimo uma data de intervenção na página inicial (apresentação).",
          type = "warning",
          size = "m")
      }
    })


    ## Atualizando o título dos cabeçalhos dos box com os gráficos de acordo com as opções do usuário na apresentação ----------


    # observeEvent(input$gerar_graficos, {
    #   output$info_user <- renderText({
    #
    #     ifelse(
    #       is.null(opcoes_usuario$date_intervention[1]), "Resultados Gerais",
    #       ifelse(
    #         is.na(opcoes_usuario$date_intervention[2]),
    #         paste(
    #           "Resultados Gerais para:", opcoes_usuario$nivel_geografico_nome,
    #           "-",opcoes_usuario$escolha_usuario,
    #           ". Data da intervenção 1:", format(opcoes_usuario$date_intervention[1],"%b/%Y")),
    #         paste(
    #           "Resultados Gerais para:", opcoes_usuario$nivel_geografico_nome,
    #           "-",opcoes_usuario$escolha_usuario,
    #           ". Data da intervenção 1:", format(opcoes_usuario$date_intervention[1],"%b/%Y"),
    #           ". Data da intervenção 2:", format(opcoes_usuario$date_intervention[2],"%b/%Y"))))
    #
    #   })
    # })

    output$info_user_geral <- renderText({
      titulo_box(data1 = opcoes_usuario$date_intervention[1],
                 data2 = opcoes_usuario$date_intervention[2],
                 nivel_geografico_nome = opcoes_usuario$nivel_geografico_nome,
                 escolha_usuario = opcoes_usuario$escolha_usuario,
                 texto = "Resultados Gerais:")

    })

    output$info_user_sexo <- renderText({
      titulo_box(data1 = opcoes_usuario$date_intervention[1],
                 data2 = opcoes_usuario$date_intervention[2],
                 nivel_geografico_nome = opcoes_usuario$nivel_geografico_nome,
                 escolha_usuario = opcoes_usuario$escolha_usuario,
                 texto = "Resultados por sexo:")

    })

    output$info_user_raca <- renderText({
      titulo_box(data1 = opcoes_usuario$date_intervention[1],
                 data2 = opcoes_usuario$date_intervention[2],
                 nivel_geografico_nome = opcoes_usuario$nivel_geografico_nome,
                 escolha_usuario = opcoes_usuario$escolha_usuario,
                 texto = "Resultados por raça:")

    })

    output$info_user_idade <- renderText({
      titulo_box(data1 = opcoes_usuario$date_intervention[1],
                 data2 = opcoes_usuario$date_intervention[2],
                 nivel_geografico_nome = opcoes_usuario$nivel_geografico_nome,
                 escolha_usuario = opcoes_usuario$escolha_usuario,
                 texto = "Resultados por idade:")

    })

    output$info_user_mortalidade <- renderText({
      titulo_box(data1 = opcoes_usuario$date_intervention[1],
                 data2 = opcoes_usuario$date_intervention[2],
                 nivel_geografico_nome = opcoes_usuario$nivel_geografico_nome,
                 escolha_usuario = opcoes_usuario$escolha_usuario,
                 texto = "Resultados por mortalidade:")

    })



    dataCategorica <- eventReactive(input$gerar_graficos, {
      dados_sim_neonatal_intervention %>%
        data_prep(nivel_geografico = opcoes_usuario$nivel_geografico, local=opcoes_usuario$escolha_usuario)
    })

    # Série temporal altera-se de acordo com as opções selecionadas pelo user e após clicar em gerar gráfico
    data <- eventReactive(input$gerar_graficos, {
      dataCategorica() %>%
        return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")
    })

    # Título da série altera-se de acordo com as opções selecionadas pelo user e após clicar em gerar gráfico
    titulo <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SIM NEONATAL) Análise de impacto com tendência no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SIM NEONATAL) Análise de impacto com tendência na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SIM NEONATAL) Análise de impacto com tendência na Regional de Saúde",opcoes_usuario$escolha_usuario),paste("(SIM NEONATAL) Análise de impacto com tendência no Município de", opcoes_usuario$escolha_usuario))))
    })

    titulo_sexo <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SIM NEONATAL) Análise de impacto na tendência por sexo no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SIM NEONATAL) Análise de impacto na tendência por sexo na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SIM NEONATAL) Análise de impacto na tendência por sexo na Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("(SIM NEONATAL) Análise de impacto na tendência por sexo no Município de", opcoes_usuario$escolha_usuario))))
    })

    titulo_tipo_mortalidade <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SIM NEONATAL) Análise de impacto na tendência por tipo de mortalidade no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SIM NEONATAL) Análise de impacto na tendência por tipo de mortalidade  na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SIM NEONATAL) Análise de impacto na tendência por tipo de mortalidade  na Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("(SIM NEONATAL) Análise de impacto na tendência por tipo de mortalidade  no Município de", opcoes_usuario$escolha_usuario))))
    })

    titulo_raca <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SIM NEONATAL) Análise de impacto na tendência por raça do recém-nascido no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SIM NEONATAL) Análise de impacto na tendência por raça do recém-nascido na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SIM NEONATAL) Análise de impacto na tendência por raça do recém-nascido  na Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("(SIM NEONATAL) Análise de impacto na tendência por raça do recém-nascido no Município de", opcoes_usuario$escolha_usuario))))
    })


    # Gráficos -------

    ## Geral -------
    output$plot_geral <- plotly::renderPlotly({
      req(opcoes_usuario$date_intervention[1])
      grafico_analise_impacto(dados = data() ,
                              titulo = titulo(),
                              ylabel = "Óbitos Neonatais",
                              intervention1 = opcoes_usuario$date_intervention[1],
                              intervention2 = opcoes_usuario$date_intervention[2])
    }) %>%
      bindCache(opcoes_usuario$escolha_usuario, opcoes_usuario$date_intervention[1], opcoes_usuario$date_intervention[2],
                input$interv_sim_neonatal) %>%
      bindEvent(input$gerar_graficos)

    ## Sexo -------
    observeEvent(input$gerar_resultado_sexo, {
      output$plot_sexo <- plotly::renderPlotly({
        req(opcoes_usuario$date_intervention[1])
        grafico_analise_impacto_sexo(dados = dataCategorica(),
                                     titulo = titulo_sexo(),
                                     ylabel = "Óbitos Neonatais",
                                     intervention1 = opcoes_usuario$date_intervention[1],
                                     intervention2 = opcoes_usuario$date_intervention[2])
      }) %>%
        bindCache(opcoes_usuario$escolha_usuario, opcoes_usuario$date_intervention[1], opcoes_usuario$date_intervention[2],
                  input$interv_sim_neonatal) %>%
        bindEvent(input$gerar_resultado_sexo)})

    ## Tipo mortalidade -------
    observeEvent(input$gerar_resultado_tipo_mortalidade, {
      output$plot_idade <- plotly::renderPlotly({
        req(opcoes_usuario$date_intervention[1])
        grafico_analise_impacto_tipo_mortalidade(dados = dataCategorica(),
                                                 titulo = titulo_tipo_mortalidade(),
                                                 ylabel = "Óbitos Neonatais",
                                                 intervention1 = opcoes_usuario$date_intervention[1],
                                                 intervention2 = opcoes_usuario$date_intervention[2])
      }) %>%
        bindCache(opcoes_usuario$escolha_usuario,
                  opcoes_usuario$date_intervention[1],
                  opcoes_usuario$date_intervention[2],
                  input$interv_sim_neonatal) %>%
        bindEvent(input$gerar_resultado_tipo_mortalidade)})


    ## Raça -------
    observeEvent(input$gerar_resultado_raca, {
      output$plot_raca <- plotly::renderPlotly({
        req(opcoes_usuario$date_intervention[1])
        grafico_analise_impacto_raca(dados = dataCategorica(),
                                     titulo = titulo_raca(),
                                     ylabel = "Óbitos Neonatais",
                                     intervention1 = opcoes_usuario$date_intervention[1],
                                     intervention2 = opcoes_usuario$date_intervention[2])
      }) %>%
        bindCache(opcoes_usuario$escolha_usuario, opcoes_usuario$date_intervention[1],opcoes_usuario$date_intervention[2],
                  input$interv_sim_neonatal) %>%
        bindEvent(input$gerar_resultado_raca)})

    # Tabelas ------

    ## Com resultados  globais ------
    tabela <- eventReactive(input$gerar_graficos, {
      tabela_intervencao(data(),opcoes_usuario$date_intervention[1],opcoes_usuario$date_intervention[2])
    })

    output$info_modelo_ajustado <- renderText({
      req(opcoes_usuario$date_intervention[1])
      tabela()
    })
    ## Com resultados  por sexo------

    # toListen <- reactive({
    #   list(input$sexo,input$gerar_resultado_sexo)
    # })

    output$info_modelo_ajustado_sexo <- renderText({
      req(input$gerar_resultado_sexo,opcoes_usuario$date_intervention[1])
      dataCategorica() %>%
        dplyr::filter(sexo==input$sexo) %>%
        return_ts(data_variable,inicio = c(2015,1), tipo = "mensal") %>%
        tabela_intervencao(opcoes_usuario$date_intervention[1],opcoes_usuario$date_intervention[2])})


    ## Com resultados  por tipo_mortalidade------

    output$info_modelo_ajustado_tipo_mortalidade <- renderText({
      req(input$gerar_resultado_tipo_mortalidade)
      dataCategorica() %>%
        dplyr::filter(tipo_mortalidade==input$tipo_mortalidade) %>%
        return_ts(data_variable,inicio = c(2015,1), tipo = "mensal") %>%
        tabela_intervencao(opcoes_usuario$date_intervention[1],opcoes_usuario$date_intervention[2])})


    ## Com resultados  por raça/cor-------

    # Quantidade de não informado por raca de  acordo com opcoes selecionadas por usuário
    na_raca <- eventReactive(input$gerar_resultado_raca, {

      denominador = dataCategorica() %>%
        nrow()

      numerador = dataCategorica() %>%
        dplyr::filter(raca_cor == "Não informado") %>%
        nrow()

      round((numerador / denominador)*100,2)

    })

    output$info_modelo_ajustado_raca <- renderText({
      req(input$gerar_resultado_raca,opcoes_usuario$date_intervention[1])
      dataCategorica() %>%
        dplyr::filter(raca_cor == input$raca) %>%
        return_ts(data_variable, inicio = c(2015,1), tipo = "mensal") %>%
        tabela_intervencao(opcoes_usuario$date_intervention[1],opcoes_usuario$date_intervention[2],
                           na = na_raca())})


    # Relatório com análise dos resíduos ----------------------------------------

    # Após clicar em gerar gráfico o bottom do relatório aparecerá apenas se o usuário selecionar no
    # mínimo uma data de intervenção
    observeEvent(input$gerar_graficos, {
      if (length(opcoes_usuario$date_intervention[1])==0)
        # Botão gerar análise de resíduos fica oculto
        shinyjs::hide("relatorio")
      else
        # Botão gerar análise de resíduos surge para o usuário
        shinyjs::show("relatorio")
    })

    output$relatorio <- downloadHandler(

      #Nome do arquivo no html
      filename <-  "Análise de resíduos (SIM-Neonatal).html",

      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).

        tempReport <- file.path(tempdir(), "relatorio.Rmd")
        file.copy("relatorio.Rmd",
                  tempReport,
                  overwrite = TRUE)


        titulo_relatorio <- eventReactive(input$gerar_graficos, {
          ifelse(is.na(opcoes_usuario$date_intervention[2]),
                 paste("Análise de resíduos, dados do SIM-Neonatal e data de intervenção:",
                       opcoes_usuario$date_intervention[1]),
                 paste("Análise de resíduos, dados do SIM-Neonatal e datas de intervenção:",
                       opcoes_usuario$date_intervention[1], "e",
                       opcoes_usuario$date_intervention[2]))
        })

        local <-  eventReactive(input$gerar_graficos, {
          ifelse(opcoes_usuario$nivel_geografico=="PR","Estado do Paraná",
                 ifelse(opcoes_usuario$nivel_geografico=="macro",paste("Macrorregião",opcoes_usuario$escolha_usuario),
                        ifelse(opcoes_usuario$nivel_geografico=="micro",paste("Regional de Saúde",opcoes_usuario$escolha_usuario),
                               paste("Município de", opcoes_usuario$escolha_usuario))))
        })

        # Set up parameters to pass to Rmd document
        params <- list(intervencao1 = opcoes_usuario$date_intervention[1],
                       intervencao2 = opcoes_usuario$date_intervention[2],
                       #Título no cabeçalho do Relatório:
                       titulo = titulo_relatorio(),
                       data_set = data(),
                       local = local() )

        # Notificação para o usuário
        id <- showNotification(
          "Gerando análise dos resíduos..",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)


        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).

        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )

      }
    )

  }
  )
}

## To be copied in the UI
# mod_intervencao_sim_neonatal_ui("intervencao_sim_neonatal_1")

## To be copied in the server
# mod_intervencao_sim_neonatal_server("intervencao_sim_neonatal_1")
