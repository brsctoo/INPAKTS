#' graficos_analise_impacto
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#'
#' @noRd
#' Função gerar o gráfico com as linhas de tendências estimadas na aba análise de intervenção
#'
#' @param dados inteiros com valores observados da serie temporal de acordo com o filtro criado pelo usuário
#' @param titulo título do gráfico gerado
#' @param tipo char se 'markers', então gráfico de dispersão. Se "lines", então gráfico de linhas
#' @importFrom magrittr "%>%"
#' @import plotly
#' @import dplyr
#'
#' @export

require(plotly)


grafico_analise_impacto <- function(dados,
                                    titulo = "(SINASC) Análise de impacto com tendência",
                                    #tipo = "lines",
                                    ylabel = "Nascidos vivos",
                                    intervention1 = "2017-03-01",
                                    intervention2 = "2021-03-01",
                                    n_previsoes = 6,
                                    min_observacoes = 0){

  stopifnot(is.numeric(dados),is.character(titulo), is.character(ylabel), is.numeric(n_previsoes))
  # Argumentos da função:
  # dados (inteiro) = inteiros com valores observados da serie temporal de acordo com o filtro criado pelo usuário
  # titulo (char)   = título do gráfico gerado
  # n_previsoes (inteiro) = quantidade de previsões a frente, default são 6 meses
  # intervention1 = data da primeira intervenção
  # intervention2 = data da segunda intervenção
  # min_observacoes (numero) = quantidade  mínima de observações na série para retornar gráfico vazio

  #Se o vetor numérico dados não tiver informação, então plota gráfico vazio
  if(sum(dados) <=  min_observacoes ){
    return(invisible())
    #Retorna gráfico tipo plotly vazio
    #return(empty_plot())
  }

  st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                 to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                 by = "1 month"))
  n <- nrow(st)

  # Ajuste para obter o log mesmo no caso de a série temporal conter zero
  if(any(dados==0)){
    dados <- (dados + min(dados[dados > 0])/2)
  }

  if(is.na(intervention2)){

    #Data das intervenções
    intervention1_date = as.Date(intervention1)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                   to = intervention1_date,
                                   by = "1 month") %>% length()


    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1),
                                 n=n,
                                 degree=c(1,1))

    # Ajustando o modelo
    dados1 <- log(dados)
    fit_lm <- lm(dados1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                     to = as.Date(max(dados_sinasc_intervencao$data_variable)) + months(n_previsoes-1),
                     by = "1 month")

    dados11 <- data.frame(data= data,dados = c(dados[1:n],rep(NA,n_previsoes)))

    # Gerando a reta de tendência
    tendencia <- exp(fitted(fit_lm))

    dados2 <- data.frame(dados11, tendencia = c(tendencia,rep(NA,n_previsoes)))

    #Previsoes para seis meses
    colnames(x) <- c("x1","x2")
    fit_lm_previsao <- lm(dados1~x1+x2,data = data.frame(dados1,x))
    new = data.frame(x1 = (n+1):(n+n_previsoes) ,x2 = (x[n,2]+1):(x[n,2]+n_previsoes))

    ## valores previstos para n_previsoes meses

    previsao = exp(predict(fit_lm_previsao,new))

    # previsoes = data.frame(exp(predict(fit_lm_previsao,new))) %>%
    #   dplyr::mutate(datas_futuras = seq.Date(from = as.Date(max(dados_sinasc_intervencao$data_variable)),
    #                                          to = as.Date(max(dados_sinasc_intervencao$data_variable)) + months(n_previsoes - 1),
    #                                          by = "1 month")) %>%
    #   dplyr::rename(previsoes = "exp.predict.fit_lm_previsao..new..")

    dados2 <- data.frame(dados2, previsao = c(rep(NA,n),previsao))

    fig <- plotly::plot_ly(
      data= dados2,
      x  = ~ data ,
      y  = ~ dados,
      color = I('#fc9272'),
      name = "Dados",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de tendência:
      add_trace(data = dados2,
                y = ~ tendencia,
                x = ~ data,
                name = 'Tendência',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta de previsão:
      add_trace(data = dados2,
                y = ~ previsao,
                x = ~ data,
                name = 'Predição para 6 meses',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#986AFC",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta horizontal na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(dados),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta horizontal na data a partir da previsão:
      # add_lines(
      #   #Legenda para a data da intervenção:
      #   name = ~"Início da predição",
      #   #A reta permanece fixa em relação ao eixo y:
      #   y = range(dados),
      #   #Data da intervenção
      #   x = as.Date(max(dados_sinasc_intervencao$data_variable)-months(1)),
      #   type = "scatter",
      #   line = list(
      #     color = "black"
    #   ),
    #   inherit = FALSE,
    #   showlegend = TRUE) %>%
    #Configurações de layout do gráfico:
    layout(
      #Posição da legenda:
      #legend = list(x = 0.1, y = 0.9),
      #Título do gráfico:
      title = paste('<b>',titulo,'</b>'),
      #Cor de fundo do gráfico:
      plot_bgcolor = "white",
      #Título do eixo x:
      xaxis = list(title = 'Ano'),
      #Título do eixo y:
      yaxis = list(title = ylabel))

  }else{
    #Data das intervenções
    intervention1_date = as.Date(intervention1)
    intervention2_date = as.Date(intervention2)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()
    tIntervention2     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention2_date,
                                   by = "1 month") %>% length()

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1,tIntervention2),
                                 n=n,
                                 degree=c(1,1,1))

    # Ajustando o modelo
    dados1 <- log(dados)
    fit_lm <- lm(dados1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                     to = as.Date(max(dados_sinasc_intervencao$data_variable)) + months(n_previsoes-1),
                     by = "1 month")

    dados11 <- data.frame(data= data,dados = c(dados[1:n],rep(NA,n_previsoes)))


    # Gerando a reta de tendência
    tendencia <- exp(fitted(fit_lm))

    dados2 <- data.frame(dados11, tendencia = c(tendencia,rep(NA,n_previsoes)))

    # Previsoes para seis meses
    colnames(x) <- c("x1","x2", "x3")
    fit_lm_previsao <- lm(dados1~x1+x2+x3,data = data.frame(dados1,x))
    new = data.frame(x1 = (n+1):(n+n_previsoes) ,x2 = (x[n,2]+1):(x[n,2]+n_previsoes), x3 = (x[n,3]+1):(x[n,3]+n_previsoes))

    # valores previstos para n_previsoes meses
    previsao = exp(predict(fit_lm_previsao,new))

    dados2 <- data.frame(dados2, previsao = c(rep(NA,n),previsao))

    fig <- plotly::plot_ly(
      data= dados2,
      x  = ~ data ,
      y  = ~ dados,
      color = I('#fc9272'),
      name = 'Dados',
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de tendência:
      add_trace(data = dados2,
                y = ~ tendencia,
                x = ~ data,
                name = 'Tendência',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta de previsão:
      add_trace(data = dados2,
                y = ~ previsao,
                x = ~ data,
                name = 'Predição para 6 meses',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#986AFC",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(dados),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta vertical na data do início da intervention2:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 2",
        #A reta permanece fixa em relação ao eixo y:
        y = range(dados),
        #Data da intervenção
        x = intervention2_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta vertical na data a partir da previsão:
      # add_lines(
      #   #Legenda para a data da intervenção:
      #   name = ~"Início da predição",
      #   #A reta permanece fixa em relação ao eixo y:
      #   y = range(dados),
      #   #Data da intervenção
      #   x = as.Date(max(dados_sinasc_intervencao$data_variable)-months(1)),
      #   type = "scatter",
      #   line = list(
      #     color = "black"
    #   ),
    #   inherit = FALSE,
    #   showlegend = TRUE) %>%
    #Configurações de layout do gráfico:
    layout(
      #Posição da legenda:
      #legend = list(x = 0.1, y = 0.9),
      #Título do gráfico:
      title = paste('<b>',titulo,'</b>'),
      #Cor de fundo do gráfico:
      plot_bgcolor = "white",
      #Título do eixo x:
      xaxis = list(title = 'Ano'),
      #Título do eixo y:
      yaxis = list(title = ylabel))
  }

  return(fig)

}

grafico_analise_impacto_sexo <- function(dados,
                                         titulo = "(SINASC) Análise de impacto com tendência por sexo",
                                         #tipo = "lines",
                                         ylabel = "Nascidos vivos",
                                         intervention1 = "2017-03-01",
                                         intervention2 = "2021-03-01",
                                         min_observacoes = 0){

  stopifnot(is.data.frame(dados),is.character(titulo), is.character(ylabel))
  # Argumentos da função:
  #dados (data.frame) = dados filtrados de acordo com a seleção do usuário para o nível geográfico e o local
  #titulo (char)   = título do gráfico gerado
  #intervention1 (char)     =  data da primeira intervenção
  #intervention2 (char)     =  data da segunda intervenção
  # min_observacoes (numero) = quantidade  mínima de observações em cada série para retornar gráfico vazio

  #data.frame dados com zero linha, então plota gráfico vazio
  # if(nrow(dados) == 0 ){
  #   return(invisible())
  #   #Retorna gráfico tipo plotly vazio
  #   #return(empty_plot())
  # }

  masculino_st <- dados %>%
    dplyr::filter(sexo == "Masculino")

  feminino_st <- dados %>%
    dplyr::filter(sexo == "Feminino")

  #Se não há observações para no mínimo um dos sexo, então plota gráfico vazio
  if(nrow(masculino_st) <= min_observacoes |  nrow(feminino_st) <= min_observacoes ){
    return(invisible())
    #Retorna gráfico tipo plotly vazio
    #return(empty_plot())
  }

  #Criando vetores numéricos para cada um dos sexos:
  masculino_st <- masculino_st %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  feminino_st <- feminino_st %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")



  # Ajuste para obter o log mesmo no caso de a série temporal conter zero
  if(any(masculino_st==0)){
    masculino_st <- (masculino_st + min(masculino_st[masculino_st > 0])/2)
  }

  if(any(feminino_st==0)){
    feminino_st <- (feminino_st + min(feminino_st[feminino_st > 0])/2)
  }

  st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                 to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                 by = "1 month"))
  n <- nrow(st)
  if(is.na(intervention2)){
    #Data das intervenções
    intervention1_date = as.Date(intervention1)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()


    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1),
                                 n=n,
                                 degree=c(1,1))

    # Ajustando o modelo
    masculino_st1 <- log(masculino_st)
    feminino_st1 <- log(feminino_st)

    fit_lm_m <- lm(masculino_st1~x)
    fit_lm_f <- lm(feminino_st1~x)
    #

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_m = masculino_st, dados_f = feminino_st)
    # dados1 <- data.frame(data= data, dados = dados) %>%
    #   #Variável intervencao = "Pós-internvenção" se a data é após intervention1_date e
    #   # "Pré-intervenção" caso contrário:
    #   dplyr::mutate(intervencao = ifelse(data < intervention1_date, "Pré Intervenção",
    #                                      ifelse(data > intervention1_date & data < intervention2_date,"Pós Intervenção 1",
    #                                             ifelse(data > intervention2_date, "Pós Intervenção 2",NA))))
    #
    # dados_pre_intervention <- dados1 %>% dplyr::filter( data <= intervention1_date)
    # dados_pos_intervention1 <- dados1 %>% dplyr::filter( data >= intervention1_date&data <= intervention2_date)
    # dados_pos_intervention2 <- dados1 %>% dplyr::filter(data >= intervention2_date)

    # Gerando a reta de tendência
    tendencia_m <- exp(fitted(fit_lm_m))
    tendencia_f <- exp(fitted(fit_lm_f))

    # tendencia_pre_intervention <- fitted(fit_lm)[1:tIntervention1]
    # tendencia_pos_intervention1 <- fitted(fit_lm)[tIntervention1:tIntervention2]
    # tendencia_pos_intervention2 <- fitted(fit_lm)[tIntervention2:n]

    dados2 <- data.frame(dados1, tendencia_m, tendencia_f)
    # dados_pre_intervention <- data.frame(dados_pre_intervention,tendencia_pre_intervention)
    # dados_pos_intervention1 <- data.frame(dados_pos_intervention1,tendencia_pos_intervention1)
    # dados_pos_intervention2 <- data.frame(dados_pos_intervention2,tendencia_pos_intervention2)
    #

    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_m,
      color = I('#fc9272'),
      name = "Masculino",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_m,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de tendência Masculino:
      add_trace(data = dados2,
                y = ~ tendencia_m,
                x = ~ data,
                name = 'Tendência Masculino',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_f,
                color = I('#6BAED6'),
                name = "Feminino",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_f,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Feminino:
      add_trace(data = dados2,
                y = ~ tendencia_f,
                x = ~ data,
                name = 'Tendência Feminino',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #,staticPlot=TRUE  torna o gráfico estático
      #Adiconando a reta horizontal na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(dados1$dados_m),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      # showlegend = TRUE)%>%
      # #Adiconando a reta de tendência pos_intervention1:
      # add_trace(data = dados_pos_intervention1,
      #           y = ~tendencia_pos_intervention1,
      #           x = ~data,
      #           name = 'Tendência pós Intervenção 1',
      #           mode = 'lines',
      #           type = "scatter",
      #           line = list(
      #             color = "#6BAED6"
      #           ),
    #           inherit = FALSE,
    #           showlegend = TRUE) %>%
    # #Adiconando a reta de tendência pos_intervention2:
    # add_trace(data = dados_pos_intervention2,
    #           y = ~tendencia_pos_intervention2,
    #           x = ~data,
    #           name = 'Tendência pós Intervenção 2',
    #           mode = 'lines',
    #           type = "scatter",
    #           line = list(
    #             color = "Feminino"
    #           ),
    #           inherit = FALSE,
    #           showlegend = TRUE) %>%
    #Configurações de layout do gráfico:
    layout(
      #Posição da legenda:
      #legend = list(x = 0.1, y = 0.9),
      #Título do gráfico:
      title = paste('<b>',titulo,'</b>'),
      #Cor de fundo do gráfico:
      plot_bgcolor = "white",
      #Título do eixo x:
      xaxis = list(title = 'Ano'),
      #Título do eixo y:
      yaxis = list(title = ylabel))

  }else{
    #Data das intervenções
    intervention1_date = as.Date(intervention1)
    intervention2_date = as.Date(intervention2)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()
    tIntervention2     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention2_date,
                                   by = "1 month") %>% length()

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1,tIntervention2),
                                 n=n,
                                 degree=c(1,1,1))

    # Ajustando o modelo
    masculino_st1 <- log(masculino_st)
    feminino_st1 <- log(feminino_st)

    fit_lm_m <- lm(masculino_st1~x)
    fit_lm_f <- lm(feminino_st1~x)


    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_m = masculino_st, dados_f = feminino_st)
    # dados1 <- data.frame(data= data, dados = dados) %>%
    #   #Variável intervencao = "Pós-internvenção" se a data é após intervention1_date e
    #   # "Pré-intervenção" caso contrário:
    #   dplyr::mutate(intervencao = ifelse(data < intervention1_date, "Pré Intervenção",
    #                                      ifelse(data > intervention1_date & data < intervention2_date,"Pós Intervenção 1",
    #                                             ifelse(data > intervention2_date, "Pós Intervenção 2",NA))))
    #
    # dados_pre_intervention <- dados1 %>% dplyr::filter( data <= intervention1_date)
    # dados_pos_intervention1 <- dados1 %>% dplyr::filter( data >= intervention1_date&data <= intervention2_date)
    # dados_pos_intervention2 <- dados1 %>% dplyr::filter(data >= intervention2_date)

    # Gerando a reta de tendência
    tendencia_m <- exp(fitted(fit_lm_m))
    tendencia_f <- exp(fitted(fit_lm_f))

    # tendencia_pre_intervention <- fitted(fit_lm)[1:tIntervention1]
    # tendencia_pos_intervention1 <- fitted(fit_lm)[tIntervention1:tIntervention2]
    # tendencia_pos_intervention2 <- fitted(fit_lm)[tIntervention2:n]

    dados2 <- data.frame(dados1, tendencia_m, tendencia_f)
    # dados_pre_intervention <- data.frame(dados_pre_intervention,tendencia_pre_intervention)
    # dados_pos_intervention1 <- data.frame(dados_pos_intervention1,tendencia_pos_intervention1)
    # dados_pos_intervention2 <- data.frame(dados_pos_intervention2,tendencia_pos_intervention2)
    #

    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_m,
      color = I('#fc9272'),
      name = 'Masculino',
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_m,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de tendência Masculino:
      add_trace(data = dados2,
                y = ~ tendencia_m,
                x = ~ data,
                name = 'Tendência Masculino',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_f,
                color = I('#6BAED6'),
                name = 'Feminino',
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_f,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Feminino:
      add_trace(data = dados2,
                y = ~ tendencia_f,
                x = ~ data,
                name = 'Tendência Feminino',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>% #,staticPlot=TRUE  torna o gráfico estático
      #Adiconando a reta horizontal na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(dados1$dados_m),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta horizontal na data do início da intervention2:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 2",
        #A reta permanece fixa em relação ao eixo y:
        y = range(dados1$dados_m),
        #Data da intervenção
        x = intervention2_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      # showlegend = TRUE)%>%
      # #Adiconando a reta de tendência pos_intervention1:
      # add_trace(data = dados_pos_intervention1,
      #           y = ~tendencia_pos_intervention1,
      #           x = ~data,
      #           name = 'Tendência pós Intervenção 1',
      #           mode = 'lines',
      #           type = "scatter",
      #           line = list(
      #             color = "#6BAED6"
      #           ),
    #           inherit = FALSE,
    #           showlegend = TRUE) %>%
    # #Adiconando a reta de tendência pos_intervention2:
    # add_trace(data = dados_pos_intervention2,
    #           y = ~tendencia_pos_intervention2,
    #           x = ~data,
    #           name = 'Tendência pós Intervenção 2',
    #           mode = 'lines',
    #           type = "scatter",
    #           line = list(
    #             color = "Feminino"
    #           ),
    #           inherit = FALSE,
    #           showlegend = TRUE) %>%
    #Configurações de layout do gráfico:
    layout(
      #Posição da legenda:
      #legend = list(x = 0.1, y = 0.9),
      #Título do gráfico:
      title = paste('<b>',titulo,'</b>'),
      #Cor de fundo do gráfico:
      plot_bgcolor = "white",
      #Título do eixo x:
      xaxis = list(title = 'Ano'),
      #Título do eixo y:
      yaxis = list(title = ylabel))
  }

  return(fig)

}



grafico_analise_impacto_idade <- function(dados,
                                          titulo = "(SINASC) Análise de impacto com tendência por idade",
                                          #tipo = "lines",
                                          ylabel = "Nascidos vivos",
                                          intervention1 = "2017-03-01",
                                          intervention2 = "2021-03-01",
                                          dados_sinasc = 100000,
                                          min_observacoes = 0){

  stopifnot(is.data.frame(dados),is.character(titulo), is.character(ylabel))
  # Argumentos da função:
  # dados (data.frame) = dados filtrados de acordo com a seleção do usuário para o nível geográfico e o local
  # titulo (char)   = título do gráfico gerado
  # intervention1 (char)   =  data da primeira intervenção
  # intervention2 (char)   =  data da segunda intervenção
  # min_observacoes (numero) = quantidade mínima de observações em cada série

  #Se o vetor numérico dados não tiver informação, então plota gráfico vazio
  if(nrow(dados) <=  min_observacoes ){
    return(invisible())
    #Retorna gráfico tipo plotly vazio
    #return(empty_plot())
  }

  jovem_st <- dados %>%
    dplyr::filter(idade == "Jovens: 10 a 18 anos") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  jovem_st <-   (jovem_st / dados_sinasc) * 100000

  adulto_jovem_st <- dados %>%
    dplyr::filter(idade  == "Adultos Jovens: 19 a 30 anos") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  adulto_jovem_st <-   (adulto_jovem_st / dados_sinasc) * 100000

  adulto_st <- dados %>%
    dplyr::filter(idade  == "Adultos: 31 a 59 anos") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  adulto_st <-   (adulto_st / dados_sinasc) * 100000

  #Se não tiver observações para as series, então plota gráfico vazio
  if(sum(adulto_st) <=  min_observacoes |  sum(adulto_jovem_st) <=  min_observacoes |  sum(jovem_st) <=  min_observacoes ){
    return(invisible())
    #return(empty_plot())
  }


  st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                 to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                 by = "1 month"))
  n <- nrow(st)

  # Ajuste para obter o log mesmo no caso de a série temporal conter zero
  if(any(jovem_st==0)){
    jovem_st <- (jovem_st + min(jovem_st[jovem_st > 0])/2)
  }
  if(any(adulto_jovem_st==0)){
    adulto_jovem_st <- (adulto_jovem_st + min(adulto_jovem_st[adulto_jovem_st > 0])/2)
  }
  if(any(adulto_st==0)){
    adulto_st <- (adulto_st + min(adulto_st[adulto_st > 0])/2)
  }


  if(is.na(intervention2)){
    #Data das intervenções
    intervention1_date = as.Date(intervention1)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()


    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1),
                                 n=n,
                                 degree=c(1,1))

    # Ajustando o modelo
    jovem_st1 <- log(jovem_st)
    adulto_jovem_st1 <- log(adulto_jovem_st)
    adulto_st1 <- log(adulto_st)
    fit_lm_j <- lm(jovem_st1~x)
    fit_lm_aj <- lm(adulto_jovem_st1~x)
    fit_lm_a <- lm(adulto_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = jovem_st, dados_aj = adulto_jovem_st,
                         dados_a = adulto_st)

    # Gerando a reta de tendência
    tendencia_j <- exp(fitted(fit_lm_j))
    tendencia_aj <- exp(fitted(fit_lm_aj))
    tendencia_a <- exp(fitted(fit_lm_a))

    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj, tendencia_a)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#33CC66'),
      name = "Jovens: 10 a 18 anos",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de jovens: 10 a 18 anos:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência Jovens: 10 a 18 anos',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#33CC66",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie de Adultos Jovens: 19 a 30 anos:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "Adultos Jovens: 19 a 30 anos",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Adultos Jovens: 19 a 30 anos:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência Adultos Jovens: 19 a 30 anos',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie Adultos:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_a,
                color = I("#fc9272"),
                name = "Adultos: 31 a 59 anos",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_a,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Adultos: 31 a 59 anos
      add_trace(data = dados2,
                y = ~ tendencia_a,
                x = ~ data,
                name = 'Tendência Adultos: 31 a 59 anos',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      # Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))

  }else{

    #Data das intervenções
    intervention1_date = as.Date(intervention1)
    intervention2_date = as.Date(intervention2)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()
    tIntervention2     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention2_date,
                                   by = "1 month") %>% length()

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1,tIntervention2),
                                 n=n,
                                 degree=c(1,1,1))

    # Ajustando o modelo
    jovem_st1 <- log(jovem_st)
    adulto_jovem_st1 <- log(adulto_jovem_st)
    adulto_st1 <- log(adulto_st)
    fit_lm_j <- lm(jovem_st1~x)
    fit_lm_aj <- lm(adulto_jovem_st1~x)
    fit_lm_a <- lm(adulto_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = jovem_st, dados_aj = adulto_jovem_st,
                         dados_a = adulto_st)

    # Gerando a reta de tendência
    tendencia_j <- exp(fitted(fit_lm_j))
    tendencia_aj <- exp(fitted(fit_lm_aj))
    tendencia_a <- exp(fitted(fit_lm_a))



    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj, tendencia_a)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#33CC66'),
      name = "Jovens: 10 a 18 anos",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de jovens: 10 a 18 anos:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência Jovens: 10 a 18 anos',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#33CC66",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie de Adultos Jovens: 19 a 30 anos:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "Adultos Jovens: 19 a 30 anos",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Adultos Jovens: 19 a 30 anos:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência Adultos Jovens: 19 a 30 anos',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie Adultos:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_a,
                color = I("#fc9272"),
                name = "Adultos: 31 a 59 anos",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_a,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Adultos: 31 a 59 anos
      add_trace(data = dados2,
                y = ~ tendencia_a,
                x = ~ data,
                name = 'Tendência Adultos: 31 a 59 anos',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta vertical na data do início da intervention2:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 2",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention2_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Configurações de layout do gráfico:
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))
  }

  return(fig)

}


grafico_analise_impacto_raca <- function(dados,
                                         titulo = "(SINASC) Análise de impacto com tendência por raça",
                                         #tipo = "lines",
                                         ylabel = "Nascidos vivos",
                                         intervention1 = "2017-03-01",
                                         intervention2 = "2021-03-01",
                                         dados_sinasc = 100000,
                                         min_observacoes = 0){

  stopifnot(is.data.frame(dados),is.character(titulo), is.character(ylabel),is.numeric(min_observacoes))
  # Argumentos da função:
  # dados (data.frame) = dados filtrados de acordo com a seleção do usuário para o nível geográfico e o local
  #titulo (char)   = título do gráfico gerado
  #intervention1 (char)   =  data da primeira intervenção
  #intervention2 (char)   =  data da segunda intervenção
  #min_observacoes (numero) = quantidade mínima de observações em cada série

  #Se o vetor numérico dados não tiver informação, então plota gráfico vazio
  # if(nrow(dados) == 0 ){
  #   return(invisible())
  #   #Retorna gráfico tipo plotly vazio
  #   #return(empty_plot())
  # }

  branca_st <- dados %>%
    dplyr::filter(raca_cor == "Branca")

  nao_branca_st <- dados %>%
    dplyr::filter(raca_cor == "Não branca")

  #Se não há observações para no mínimo uma das raças, então plota gráfico vazio
  if(nrow( branca_st) <= min_observacoes |  nrow(nao_branca_st) <= min_observacoes ){
    return(invisible())
    #Retorna gráfico tipo plotly vazio
    #return(empty_plot())
  }

  #Criando vetores numéricos para cada um das raças:
  branca_st <-  branca_st %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  nao_branca_st <- nao_branca_st %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  if(sum(branca_st) <= min_observacoes | sum(nao_branca_st) <= min_observacoes ){
    return(invisible())
    #Retorna gráfico tipo plotly vazio
    #return(empty_plot())
  }

  branca_st <-   (branca_st / dados_sinasc) * 100000

  nao_branca_st <- (nao_branca_st / dados_sinasc) * 100000


  st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                 to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                 by = "1 month"))
  n <- nrow(st)

  # Ajuste para obter o log mesmo no caso de a série temporal conter zero
  if(any(branca_st==0)){
    branca_st <- (branca_st + min(branca_st[branca_st > 0])/2)
  }

  if(any(nao_branca_st==0)){
    nao_branca_st <- (nao_branca_st + min(nao_branca_st[nao_branca_st > 0])/2)
  }


  if(is.na(intervention2)){
    #Data das intervenções
    intervention1_date = as.Date(intervention1)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()


    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1),
                                 n=n,
                                 degree=c(1,1))

    # Ajustando o modelo
    branca_st1 <- log(branca_st)
    nao_branca_st1 <- log(nao_branca_st)
    fit_lm_j <- lm(branca_st1~x)
    fit_lm_aj <- lm(nao_branca_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = branca_st, dados_aj = nao_branca_st)

    # Gerando a reta de tendência
    tendencia_j <- exp(fitted(fit_lm_j))
    tendencia_aj <- exp(fitted(fit_lm_aj))

    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#fc9272'),
      name = "Branca",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta branca:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência para raça Branca',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie da raça não branca:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "Não Branca",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de Tendência para raça não branca:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência para raça não branca',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      # Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))

  }else{

    #Data das intervenções
    intervention1_date = as.Date(intervention1)
    intervention2_date = as.Date(intervention2)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()
    tIntervention2     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention2_date,
                                   by = "1 month") %>% length()

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1,tIntervention2),
                                 n=n,
                                 degree=c(1,1,1))

    # Ajustando o modelo
    branca_st1 <- log(branca_st)
    nao_branca_st1 <- log(nao_branca_st)
    fit_lm_j <- lm(branca_st1~x)
    fit_lm_aj <- lm(nao_branca_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = branca_st, dados_aj = nao_branca_st)

    # Gerando a reta de tendência
    tendencia_j <- exp(fitted(fit_lm_j))
    tendencia_aj <- exp(fitted(fit_lm_aj))

    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj)



    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#fc9272'),
      name = "Branca",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta raça branca:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência para a raça Branca',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie da raça não branca
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "Não Branca",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de Tendência para a raça não branca:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência para a raça não branca',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta vertical na data do início da intervention2:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 2",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention2_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Configurações de layout do gráfico:
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))
  }

  return(fig)

}

grafico_analise_impacto_tipo_mortalidade <- function(dados,
                                                     titulo = "(SINASC) Análise de impacto com tendência por idade",
                                                     #tipo = "lines",
                                                     ylabel = "Nascidos vivos",
                                                     intervention1 = "2017-03-01",
                                                     intervention2 = "2021-03-01"){

  stopifnot(is.data.frame(dados),is.character(titulo), is.character(ylabel))

  # Argumentos da função:
  # dados (data.frame) = dados filtrados de acordo com a seleção do usuário para o nível geográfico e o local
  # titulo (char)   = título do gráfico gerado
  # intervention1 (char)   =  data da primeira intervenção
  # intervention2 (char)   =  data da segunda intervenção
  #min_observacoes (numero) = quantidade mínima de observações em cada série

  #Se o vetor numérico dados não tiver informação, então plota gráfico vazio
  if(nrow(dados) == 0 ){
    return(invisible())
    #Retorna gráfico tipo plotly vazio
    #return(empty_plot())
  }

  # vetores numericos
  fetal_st <- dados %>%
    dplyr::filter(tipo_mortalidade == "fetal") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  neonatal_precoce_st <- dados %>%
    dplyr::filter(tipo_mortalidade == "neonatal_precoce") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  neonatal_tardia_st <- dados %>%
    dplyr::filter(tipo_mortalidade == "neonatal_tardia") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  #Se não há observações para os vetores numéricos acima, então plota gráfico vazio
  if(sum(fetal_st) == 0 |  sum(neonatal_precoce_st) == 0 |  sum(neonatal_tardia_st) == 0 ){
    return(invisible())
    #return(empty_plot())
  }

  st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                 to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                 by = "1 month"))
  n <- nrow(st)

  # Ajuste para obter o log mesmo no caso de a série temporal conter zero
  if(any(fetal_st==0)){
    fetal_st <- (fetal_st + min(fetal_st[fetal_st > 0])/2)
  }
  if(any(neonatal_precoce_st==0)){
    neonatal_precoce_st <- (neonatal_precoce_st + min(neonatal_precoce_st[neonatal_precoce_st > 0])/2)
  }
  if(any(neonatal_tardia_st==0)){
    neonatal_tardia_st <- (neonatal_tardia_st + min(neonatal_tardia_st[neonatal_tardia_st > 0])/2)
  }

  if(is.na(intervention2)){
    #Data das intervenções
    intervention1_date = as.Date(intervention1)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()


    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1),
                                 n=n,
                                 degree=c(1,1))

    # Ajustando o modelo
    fetal_st1 <- log(fetal_st)
    neonatal_precoce_st1 <- log(neonatal_precoce_st)
    neonatal_tardia_st1 <- log(neonatal_tardia_st)
    fit_lm_f <- lm(fetal_st1~x)
    fit_lm_np <- lm(neonatal_precoce_st1~x)
    fit_lm_a <- lm(neonatal_tardia_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = fetal_st, dados_aj = neonatal_precoce_st,
                         dados_a = neonatal_tardia_st)

    # Gerando a reta de tendência
    tendencia_j <- exp(fitted(fit_lm_f))
    tendencia_aj <- exp(fitted(fit_lm_np))
    tendencia_t <- exp(fitted(fit_lm_a))

    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj, tendencia_t)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#33CC66'),
      name = "Fetal",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de Fetal:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência Fetal',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#33CC66",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie de Neonatal precoce:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "Neonatal precoce",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Neonatal precoce:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência Neonatal precoce',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie Neonatal tardia:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_a,
                color = I("#fc9272"),
                name = "Neonatal tardia",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_a,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de Tendência Adultos neonatal tardia
      add_trace(data = dados2,
                y = ~ tendencia_t,
                x = ~ data,
                name = 'Tendência Adultos neonatal tardia',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      # Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))

  }else{

    #Data das intervenções
    intervention1_date = as.Date(intervention1)
    intervention2_date = as.Date(intervention2)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()
    tIntervention2     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention2_date,
                                   by = "1 month") %>% length()

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1,tIntervention2),
                                 n=n,
                                 degree=c(1,1,1))

    # Ajustando o modelo
    fetal_st1 <- log(fetal_st)
    neonatal_precoce_st1 <- log(neonatal_precoce_st)
    neonatal_tardia_st1 <- log(neonatal_tardia_st)
    fit_lm_f <- lm(fetal_st1~x)
    fit_lm_np <- lm(neonatal_precoce_st1~x)
    fit_lm_a <- lm(neonatal_tardia_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = fetal_st, dados_aj = neonatal_precoce_st,
                         dados_a = neonatal_tardia_st)

    # Gerando a reta de tendência
    tendencia_j <- exp(fitted(fit_lm_f))
    tendencia_aj <- exp(fitted(fit_lm_np))
    tendencia_t <- exp(fitted(fit_lm_a))



    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj, tendencia_t)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#33CC66'),
      name = "Fetal",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de Fetal:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência Fetal',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#33CC66",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie de Neonatal precoce:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "Neonatal precoce",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência Neonatal precoce:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência Neonatal precoce',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie Adultos:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_a,
                color = I("#fc9272"),
                name = "Neonatal Tardia",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_a,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de Tendência Adultos neonatal tardia
      add_trace(data = dados2,
                y = ~ tendencia_t,
                x = ~ data,
                name = 'Tendência neonatal tardia',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta vertical na data do início da intervention2:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 2",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention2_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Configurações de layout do gráfico:
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))
  }

  return(fig)

}

grafico_analise_impacto_idade_sif_c <- function(dados,
                                                titulo = "(SINASC) Análise de impacto com tendência por idade",
                                                #tipo = "lines",
                                                ylabel = "Nascidos vivos",
                                                intervention1 = "2017-03-01",
                                                intervention2 = "2021-03-01",
                                                min_observacoes = 0){

  stopifnot(is.data.frame(dados),is.character(titulo), is.character(ylabel))

  # Argumentos da função:
  # dados (data.frame) = dados filtrados de acordo com a seleção do usuário para o nível geográfico e o local
  # titulo (char)   = título do gráfico gerado
  # intervention1 (char)   =  data da primeira intervenção
  # intervention2 (char)   =  data da segunda intervenção
  # min_observacoes (numero) = quantidade  mínima de observações em cada série para retornar gráfico vazio

  #Se o vetor numérico dados não tiver informação, então plota gráfico vazio
  if(nrow(dados) <= min_observacoes ){
    return(invisible())
    #Retorna gráfico tipo plotly vazio
    #return(empty_plot())
  }

  jovem_st <- dados %>%
    dplyr::filter(idade == "Menos de 7 dias") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  adulto_jovem_st <- dados %>%
    dplyr::filter(idade  == "7 a 27 dias") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  adulto_st <- dados %>%
    dplyr::filter(idade  == "28 dias a 1 ano") %>%
    return_ts(data_variable,inicio = c(2015,1), tipo = "mensal")

  #Se não tiver observações para as series, então plota gráfico vazio
  if(sum(adulto_st) <= min_observacoes |  sum(adulto_jovem_st) <= min_observacoes |  sum(jovem_st) <= min_observacoes ){
    return(invisible())
    #return(empty_plot())
  }

  st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                 to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                 by = "1 month"))
  n <- nrow(st)

  # Ajuste para obter o log mesmo no caso de a série temporal conter zero
  if(any(jovem_st==0)){
    jovem_st <- (jovem_st + min(jovem_st[jovem_st > 0])/2)
  }
  if(any(adulto_jovem_st==0)){
    adulto_jovem_st <- (adulto_jovem_st + min(adulto_jovem_st[adulto_jovem_st > 0])/2)
  }
  if(any(adulto_st==0)){
    adulto_st <- (adulto_st + min(adulto_st[adulto_st > 0])/2)
  }


  if(is.na(intervention2)){
    #Data das intervenções
    intervention1_date = as.Date(intervention1)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()


    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1),
                                 n=n,
                                 degree=c(1,1))

    # Ajustando o modelo
    jovem_st1 <- log(jovem_st)
    adulto_jovem_st1 <- log(adulto_jovem_st)
    adulto_st1 <- log(adulto_st)
    fit_lm_j <- lm(jovem_st1~x)
    fit_lm_aj <- lm(adulto_jovem_st1~x)
    fit_lm_a <- lm(adulto_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = jovem_st, dados_aj = adulto_jovem_st,
                         dados_a = adulto_st)

    # Criando as retas de tendência
    tendencia_j <- exp(fitted(fit_lm_j))
    tendencia_aj <- exp(fitted(fit_lm_aj))
    tendencia_a <- exp(fitted(fit_lm_a))

    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj, tendencia_a)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#33CC66'),
      name = "Menos de 7 dias",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de Menos de 7 dias:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência Menos de 7 dias',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#33CC66",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie de 7 a 27 dias:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "7 a 27 dias",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência 7 a 27 dias:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência 7 a 27 dias',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie Adultos:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_a,
                color = I("#fc9272"),
                name = "28 dias a 1 ano",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_a,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência 28 dias a 1 ano
      add_trace(data = dados2,
                y = ~ tendencia_a,
                x = ~ data,
                name = 'Tendência 28 dias a 1 ano',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      # Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))

  }else{

    #Data das intervenções
    intervention1_date = as.Date(intervention1)
    intervention2_date = as.Date(intervention2)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()
    tIntervention2     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention2_date,
                                   by = "1 month") %>% length()

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1,tIntervention2),
                                 n=n,
                                 degree=c(1,1,1))

    # Ajustando o modelo
    jovem_st1 <- log(jovem_st)
    adulto_jovem_st1 <- log(adulto_jovem_st)
    adulto_st1 <- log(adulto_st)
    fit_lm_j <- lm(jovem_st1~x)
    fit_lm_aj <- lm(adulto_jovem_st1~x)
    fit_lm_a <- lm(adulto_st1~x)

    data <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                     by = "1 month")

    dados1 <- data.frame(data= data, dados_j = jovem_st, dados_aj = adulto_jovem_st,
                         dados_a = adulto_st)

    # Gerando a reta de tendência
    tendencia_j <- exp(fitted(fit_lm_j))
    tendencia_aj <- exp(fitted(fit_lm_aj))
    tendencia_a <- exp(fitted(fit_lm_a))



    dados2 <- data.frame(dados1, tendencia_j, tendencia_aj, tendencia_a)


    fig <- plotly::plot_ly(
      data= dados1, x  = ~ data ,
      y  = ~ dados_j,
      color = I('#33CC66'),
      name = "Menos de 7 dias",
      #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
      type = "scatter",
      # Gráficos de dispersão:
      #mode = "markers",
      # Gráficos de linhas:
      mode = "lines",
      #Mudando a legenda:
      #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
      #Mudar o texto  quando clicamos nos pontos do gráfico:
      hoverinfo = 'text',
      text = ~paste(
        "<br>", ylabel,":", round(dados_j,3), "<br>",
        "Data: ", data, "<br>"
      )) %>%
      config(displayModeBar = FALSE) %>%
      #Adiconando a reta de Menos de 7 dias:
      add_trace(data = dados2,
                y = ~ tendencia_j,
                x = ~ data,
                name = 'Tendência Menos de 7 dias',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#33CC66",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie de 7 a 27 dias:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_aj,
                color = I("#6BAED6"),
                name = "7 a 27 dias",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_aj,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência 7 a 27 dias:
      add_trace(data = dados2,
                y = ~ tendencia_aj,
                x = ~ data,
                name = 'Tendência 7 a 27 dias',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#6BAED6",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a serie Adultos:
      add_lines(data= dados1, x  = ~ data ,
                y  = ~ dados_a,
                color = I("#fc9272"),
                name = "28 dias a 1 ano",
                #colors = c("#fc9272","#6BAED6"), ##2171B5 #6BAED6 #BDD7E7
                type = "scatter",
                # Gráficos de dispersão:
                #mode = "markers",
                # Gráficos de linhas:
                mode = "lines",
                #Mudando a legenda:
                #name = ifelse(dados$intervencao == "Observações Pós-covid", "Observações Pré-internvenção", "Observações Pós-internvenção"),
                #Mudar o texto  quando clicamos nos pontos do gráfico:
                hoverinfo = 'text',
                text = ~paste(
                  "<br>", ylabel,":", round(dados_a,3), "<br>",
                  "Data: ", data, "<br>"
                )) %>%
      #Adiconando a reta de tendência 28 dias a 1 ano
      add_trace(data = dados2,
                y = ~ tendencia_a,
                x = ~ data,
                name = 'Tendência 28 dias a 1 ano',
                mode = 'lines',
                #text = ~paste('Species: '),
                type = "scatter",
                line = list(
                  color = "#fc9272",
                  dash= 'dash'
                ),
                inherit = FALSE,
                showlegend = TRUE)%>%
      #Adiconando a reta vertical na data da intervention 1:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 1",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention1_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Adiconando a reta vertical na data do início da intervention2:
      add_lines(
        #Legenda para a data da intervenção:
        name = ~"Intervenção 2",
        #A reta permanece fixa em relação ao eixo y:
        y = range(min(dados1$dados_j,dados1$dados_aj,dados1$dados_a),max(dados1$dados_j,dados1$dados_aj,dados1$dados_a)),
        #Data da intervenção
        x = intervention2_date,
        type = "scatter",
        line = list(
          color = "black"
        ),
        inherit = FALSE,
        showlegend = TRUE) %>%
      #Configurações de layout do gráfico:
      layout(
        #Posição da legenda:
        #legend = list(x = 0.1, y = 0.9),
        #Título do gráfico:
        title = paste('<b>',titulo,'</b>'),
        #Cor de fundo do gráfico:
        plot_bgcolor = "white",
        #Título do eixo x:
        xaxis = list(title = 'Ano'),
        #Título do eixo y:
        yaxis = list(title = ylabel))
  }

  return(fig)

}



# Função para gerar a tabela com os resultados estatísticas do modelo
tabela_intervencao <- function(dados,
                               date_intervention1,
                               date_intervention2,
                               na=NULL,
                               dados_sinasc = 100000,
                               min_observacoes = 0){

  stopifnot(is.numeric(dados),is.numeric(dados_sinasc))

  # Argumentos da função:
  # dados = vetor numerico com dados de uma determinada série
  # titulo (char)   = título do gráfico gerado
  # date_intervention1    =  data da primeira intervenção
  # date_intervention2   =  data da segunda intervenção
  # na (numeric) quantidade de não informados
  # min_observacoes (numero) = quantidade mínima de observações em cada série

  #Se o vetor numérico dados não tiver informação, então plota gráfico vazio
  if(sum(dados) <= min_observacoes ){
    return(invisible())
  }

  note = ifelse(!is.null(na),paste0("* indica valor p menor que 5%. ",na,
                                    "% de valores não informados."),
                "* indica valor p menor que 5%.")

  dados <- (dados / dados_sinasc) * 100000

  if(any(dados==0)){
    dados <- (dados + min(dados[dados > 0])/2)
  }

  if(is.na(date_intervention2)){
    tibble::tibble(
      ` ` = c("Pré Interv.: ",
              "Mudança Interv. 1: ",
              "Pós Interv. 1: "),
      `Estimativa` = c(paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[1,1],3)*100),"%",
                              ifelse(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[1,3]<0.05, "*", " ")),
                       paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$fit_lm$coefficients[3],3)*100),"%",
                              ifelse(summary(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$fit_lm)$coefficients[3,4]<0.05, "*"," ")),
                       paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[2,1],3)*100),"%",
                              ifelse(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[2,3]<0.05, "*"," ")))) %>%
      kableExtra::kable("html",
                        booktabs = T,
                        align = "c",
                        caption = "") %>%
      kableExtra::kable_styling(full_width = F,font_size =13)  %>%
      kableExtra::column_spec(1, bold = F) %>%
      kableExtra::column_spec(2, width = "10em") %>%
      kableExtra::kable_styling(latex_options = c("striped", "hold_position"),full_width = F ) %>%
      kableExtra::footnote(general =  note,
                           general_title = "",
                           threeparttable = TRUE, escape = F)}
  else{
    tibble::tibble(
      ` ` = c("Pré Interv.: ",
              "Mudança Interv. 1: ",
              "Pós Interv. 1: ",
              "Mudança Interv. 2: ",
              "Pós Interv. 2: "),
      `Estimativa` = c(paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[1,1],3)*100),"%",
                              ifelse(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[1,3]<0.05, "*", " ")),
                       paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$fit_lm$coefficients[3],3)*100),"%",
                              ifelse(summary(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$fit_lm)$coefficients[3,4]<0.05, "*"," ")),
                       paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[2,1],3)*100),"%",
                              ifelse(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[2,3]<0.05, "*"," ")),
                       paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$fit_lm$coefficients[4],3)*100),"%",
                              ifelse(summary(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$fit_lm)$coefficients[4,4]<0.05, "*"," ")),
                       paste0(as.character(round(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[3,1],3)*100),"%",
                              ifelse(sinasc_modelo.ajustado(log(dados),date_intervention1,date_intervention2)$ResultingTrends[3,3]<0.05, "*"," "))
      )) %>%
      kableExtra::kable("html",
                        booktabs = T,
                        align = "c",
                        caption = "") %>%
      kableExtra::kable_styling(full_width = F,font_size =13)  %>%
      kableExtra::column_spec(1, bold = F) %>%
      kableExtra::column_spec(2, width = "10em") %>%
      kableExtra::kable_styling(latex_options = c("striped", "hold_position"),full_width = F ) %>%
      kableExtra::footnote(general =  note,
                           general_title = "",
                           threeparttable = TRUE, escape = F)
  }
}
