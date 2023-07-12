#' intervencao_sinasc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intervencao_sinasc_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(

      #Para gerar os pop-ups:

      #shinyWidgets::useSweetAlert(),

      shinyWidgets::useBs4Dash(),

      #shinyjs::useShinyjs(),

      # Opções para o usuário selecionar -------
      # fluidRow(
      #   column(2,
      #          # paste("Você selecionou duas datas de intervenção nos dias",
      #          #          textOutput(ns("date_intervention1")),"e",
      #          #          textOutput(ns("date_intervention2"))),
      #          ## Botão com informações:-------
      #          actionButton(
      #            inputId = ns("info"),
      #            label = "Informações",
      #            icon = icon("thumbs-up")
      #          ))),
      #hr()
      # h6(textOutput(ns("info_user1"))),
      hr(),
      # Layout onde gráficos e a tabelas serão exibidos -------
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
                 plotly::plotlyOutput(ns("plot_geral"))))
      ),
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
                 actionButton(inputId = ns("gerar_resultado_sexo"),
                              label = "Gerar resultados"))),
        fluidRow(
          column(2,
                 selectInput(inputId = ns("sexo"),
                             label = "Escolha o sexo",
                             choices = c("Feminino","Masculino")),
                 tableOutput((ns("info_modelo_ajustado_sexo")))),
          column(10,
                 plotly::plotlyOutput(ns("plot_sexo"))))),
      hr(),
      bs4Dash::bs4Card(
        title = textOutput(ns("info_user_idade")),#htmlOutput(ns("info_user")),
        status = "primary",
        width = 12,
        collapsed = TRUE,
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(10),
          column(2,
                 actionButton(inputId = ns("gerar_resultado_idade"),label = "Gerar resultados"))),
        fluidRow(
          column(2,
                 selectInput(inputId = ns("idade"),
                             label = "Escolha a idade",
                             choices = c("Jovens: 10 a 18 anos",
                                         "Adultos Jovens: 19 a 30 anos",
                                         "Adultos: 31 a 59 anos")),
                 tableOutput((ns("info_modelo_ajustado_idade")))),
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
                             choices = levels(dados_sinasc_intervencao$raca_cor)[1:2]),
                 tableOutput((ns("info_modelo_ajustado_raca")))),
          column(10,
                 plotly::plotlyOutput(ns("plot_raca")))))
    )
  )
}


#' intervencao_sinasc Server Functions
#'
#' @noRd
mod_intervencao_sinasc_server <- function(id, opcoes_usuario){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Pop-ups -------

    ## Pop-up surgirá se nenhuma data de intervenção for selecionada e usuario clicar no botao para gerar algum gráfico-------
    observeEvent( input$gerar_graficos, {
      if(length(opcoes_usuario$date_intervention[1])==0){
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
      if(length(opcoes_usuario$date_intervention[1])==0){
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


    # Atualizando o título dos cabeçalhos dos box com os gráficos de acordo com as opções do usuário na apresentação ----------



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



    # filtrando os dados de acordo com as opções do usuário e após usuário clicar botão Gerar gráfico
    dataCategorica <- eventReactive(input$gerar_graficos, {
      dados_sinasc_intervencao %>%
        data_prep(nivel_geografico = opcoes_usuario$nivel_geografico,
                  local = opcoes_usuario$escolha_usuario)
    })

    # Série numerica altera-se de acordo com as opções selecionadas pelo usuario e após clicar em gerar gráfico
    data <- eventReactive(input$gerar_graficos, {
      dataCategorica() %>%
        return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")
    })

    #Título da série altera-se de acordo com as opções selecionadas pelo user e após clicar em gerar gráfico
    titulo <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SINASC) Análise de impacto na tendência no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SINASC) Análise de impacto na tendência na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SINASC) Análise de impacto na tendência na Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("(SINASC) Análise de impacto na tendência no Município de", opcoes_usuario$escolha_usuario))))
    })

    titulo_sexo <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SINASC) Análise de impacto na tendência por sexo no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SINASC) Análise de impacto na tendência por sexo na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SINASC) Análise de impacto na tendência por sexo na Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("(SINASC) Análise de impacto na tendência por sexo no Município de", opcoes_usuario$escolha_usuario))))
    })

    titulo_idade <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SINASC) Análise de impacto na tendência por idade materna no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SINASC) Análise de impacto na tendência por idade materna  na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SINASC) Análise de impacto na tendência por idade materna  na Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("(SINASC) Análise de impacto na tendência por idade materna  no Município de", opcoes_usuario$escolha_usuario))))
    })

    titulo_raca <- eventReactive(input$gerar_graficos, {
      ifelse(opcoes_usuario$nivel_geografico=="PR","(SINASC) Análise de impacto na tendência por raça do recém-nascido no Paraná",
             ifelse(opcoes_usuario$nivel_geografico=="macro",paste("(SINASC) Análise de impacto na tendência por raça do recém-nascido na Macrorregião",opcoes_usuario$escolha_usuario),
                    ifelse(opcoes_usuario$nivel_geografico=="micro",paste("(SINASC) Análise de impacto na tendência por raça do recém-nascido  na Regional de Saúde",opcoes_usuario$escolha_usuario),
                           paste("(SINASC) Análise de impacto na tendência por raça do recém-nascido no Município de", opcoes_usuario$escolha_usuario))))
    })


    # Gráficos -------

      ## Geral -------
    output$plot_geral <- plotly::renderPlotly({
      req(opcoes_usuario$date_intervention[1])
      grafico_analise_impacto(dados = data() ,
                              titulo = titulo(),
                              intervention1 =  opcoes_usuario$date_intervention[1],
                              intervention2 =  opcoes_usuario$date_intervention[2])
    }) %>%
      bindCache(opcoes_usuario$escolha_usuario,
                opcoes_usuario$date_intervention[1],
                opcoes_usuario$date_intervention[2],
                input$interv_sinasc,
                data()) %>%
      bindEvent(input$gerar_graficos)

    ## Sexo -------
    observeEvent(input$gerar_resultado_sexo, {
      output$plot_sexo <- plotly::renderPlotly({
        req(opcoes_usuario$date_intervention[1])
        grafico_analise_impacto_sexo(dados = dataCategorica(),
                                     titulo = titulo_sexo(),
                                     ylabel = "Nascidos vivos",
                                     intervention1 =  opcoes_usuario$date_intervention[1],
                                     intervention2 =  opcoes_usuario$date_intervention[2])
      }) %>%
        bindCache(opcoes_usuario$escolha_usuario,
                  opcoes_usuario$date_intervention[1],
                  opcoes_usuario$date_intervention[2],
                  input$interv_sinasc) %>%
        bindEvent(input$gerar_resultado_sexo)})

    ## Idade -------
    observeEvent(input$gerar_resultado_idade, {
      output$plot_idade <- plotly::renderPlotly({
        req( opcoes_usuario$date_intervention[1])
        grafico_analise_impacto_idade(dados = dataCategorica(),
                                      titulo = titulo_idade(),
                                      ylabel = "Nascidos vivos",
                                      intervention1 =  opcoes_usuario$date_intervention[1],
                                      intervention2 =  opcoes_usuario$date_intervention[2])
      }) %>%
        bindCache(opcoes_usuario$escolha_usuario,  opcoes_usuario$date_intervention[1],
                  opcoes_usuario$date_intervention[2], input$interv_sinasc) %>%
        bindEvent(input$gerar_resultado_idade)})

    ## Raça -------
    observeEvent(input$gerar_resultado_raca, {
      output$plot_raca <- plotly::renderPlotly({
        req( opcoes_usuario$date_intervention[1])
        grafico_analise_impacto_raca(dados = dataCategorica(),
                                     titulo = titulo_raca(),
                                     ylabel = "Nascidos vivos",
                                     intervention1 =  opcoes_usuario$date_intervention[1],
                                     intervention2 =  opcoes_usuario$date_intervention[2])
      }) %>%
        bindCache(opcoes_usuario$escolha_usuario,  opcoes_usuario$date_intervention[1],
                  opcoes_usuario$date_intervention[2], input$interv_sinasc) %>%
        bindEvent(input$gerar_resultado_raca)})

    # Tabelas ------

    ## Para resultados  gerais ------
    tabela <- eventReactive(input$gerar_graficos, {
      tabela_intervencao(data(),
                         opcoes_usuario$date_intervention[1],
                         opcoes_usuario$date_intervention[2])
    })

    output$info_modelo_ajustado <- renderText({
      req( opcoes_usuario$date_intervention[1])
      tabela()
    }) %>%
      bindCache(opcoes_usuario$escolha_usuario, input$interv_sinasc,
                opcoes_usuario$date_intervention[1],
                opcoes_usuario$date_intervention[2]) %>%
      bindEvent(input$gerar_graficos)

    ## Para resultados  por sexo------

    # dataCategorica_1 <- eventReactive(input$gerar_resultado_sexo, {
    #   dados_sinasc_intervencao %>%
    #     data_prep(nivel_geografico = opcoes_usuario$nivel_geografico,
    #               local = opcoes_usuario$escolha_usuario)
    # })

    observeEvent(input$gerar_resultado_sexo, {
    output$info_modelo_ajustado_sexo <- renderText({

      req(input$gerar_resultado_sexo, opcoes_usuario$date_intervention[1],cancelOutput = FALSE)

      dataCategorica() %>%
        dplyr::filter(sexo==input$sexo) %>%
        return_ts(data_variable,inicio = c(2015,1), tipo = "mensal") %>%
        tabela_intervencao(opcoes_usuario$date_intervention[1], opcoes_usuario$date_intervention[2])})
    })

    ## Para resultados  por idade------

    observeEvent(input$gerar_resultado_idade, {
    output$info_modelo_ajustado_idade <- renderText({
      req(opcoes_usuario$date_intervention[1])
      dataCategorica() %>%
        dplyr::filter(idade==input$idade) %>%
        return_ts(data_variable,inicio = c(2015,1), tipo = "mensal") %>%
        tabela_intervencao( opcoes_usuario$date_intervention[1], opcoes_usuario$date_intervention[2])})
    })

    ## Para resultados  por raça/cor-------

    # Quantidade de não informado por raca de  acordo com opcoes selecionadas por usuário
    na_raca <- eventReactive(input$gerar_resultado_raca, {

      denominador = dataCategorica() %>%
        nrow()

      numerador = dataCategorica() %>%
        dplyr::filter(raca_cor == "Não informado") %>%
        nrow()

      round((numerador / denominador)*100,2)

    })

    observeEvent(input$gerar_resultado_raca, {
    output$info_modelo_ajustado_raca <- renderText({
      req(opcoes_usuario$date_intervention[1])
      dataCategorica() %>%
        dplyr::filter(raca_cor == input$raca) %>%
        return_ts(data_variable, inicio = c(2015,1), tipo = "mensal") %>%
        tabela_intervencao( opcoes_usuario$date_intervention[1],
                            opcoes_usuario$date_intervention[2],
                            na = na_raca())})
    })

    # Relatório dos resíduos ----------------------------------------

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
      filename <-  "Análise de resíduos (SINASC).html",

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
                 paste("Análise de resíduos, dados do SINASC e data de intervenção:",
                       opcoes_usuario$date_intervention[1]),
                 paste("Análise de resíduos, dados do SINASC e datas de intervenção:",
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
# mod_intervencao_sinasc_ui("intervencao_sinasc_1")

## To be copied in the server
# mod_intervencao_sinasc_server("intervencao_sinasc_1")
