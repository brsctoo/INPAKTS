#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# Função para gerar quebrar de linhas em arquivo html --------

linebreaks <- function(n){HTML(strrep(br(), n))}

# data_prep <- function(dados_sinasc,municipio,micro,macro){
#
#   data_grafico <- dados_sinasc %>%
#     filter(municipio == {{municipio}})# , micro == {{ micro }},macro == {{ macro }})
#   #filter(municipio == {{municipio}},micro == {{ micro }},macro == {{ macro }})
#   return(data_grafico)
# }
#


# Função selecionar subamostra do conjunto de dados de acordo com as opões geográficas selecionadas pelo usuário ----------------------

data_prep <- function(dados,nivel_geografico = "PR",local = "PR"){

  if(nivel_geografico=="PR" & local=="PR"){
    data_grafico <- dados
  }else{
    data_grafico <- dados %>%
      dplyr::filter(get(nivel_geografico) == local)
  }
  return(data_grafico)
}


# d <- data_prep(dados_sinasc=dados_sinasc,nivel_geografico = "municipio", local="Toledo")
#
# fct_count(as.factor(d$ibge_estabelecimento))
# a=fct_count(as.factor(d$municipio))
# fct_count(as.factor(d$cnes_estabelecimento))

# Gerar ST semanal ou mensal----------------
return_ts <-  function(data_prep,date_var,inicio=c(ano,sem),tipo="mensal") {

  if(tipo=="mensal"){
    # st1 <- data_prep %>%
    #   mutate(MES = lubridate::month({{date_var}})) %>%
    #   mutate(ANO = lubridate::year({{date_var}})) %>%
    #   mutate(ANO_MES = ifelse(MES < 10,
    #                           paste0(ANO,"0",MES),
    #                           paste0(ANO,MES))) %>%
    #   group_by(ANO_MES) %>%
    #   summarize(n = n())
    #dados_st <- ts(as.ts(st$n), frequency = 12, start=c(inicio))
    st1 <- data_prep %>%
      mutate(MES = lubridate::month({{date_var}})) %>%
      mutate(ANO = lubridate::year({{date_var}})) %>%
      mutate(DATA = ifelse(MES < 10,
                           paste0(ANO,"-0",MES,"-01"),
                           paste0(ANO,"-",MES,"-01"))) %>%
      mutate_at("DATA",as.Date) %>%
      group_by(DATA) %>%
      summarize(n = n())

    st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                                                  to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                                                  by = "1 month")) %>%
      dplyr::left_join(st1,by = "DATA")

    st$n <- ifelse(is.na(st$n),0,st$n)

  } else if(tipo=="semanal"){
    st <- data_prep %>%
      mutate(SEM_EPI = dengueControl::date2Week({{date_var}})$weeknum) %>%
      mutate(ANO_EPI = dengueControl::date2Week({{date_var}})$year) %>%
      mutate(ANO_SEM_EPI = ifelse(SEM_EPI < 10,
                                  paste0(ANO_EPI,"0",SEM_EPI),
                                  paste0(ANO_EPI,SEM_EPI))) %>%
      group_by(ANO_SEM_EPI) %>%
      summarize(n = n())

    #dados_st <- ts(as.ts(st$n), frequency = 52, start=c(inicio))
  }

  return(st$n)
}

#load(file = "data/dados_sim_materno_comp.RData")
#teste <- return_ts(dados_sim_materno,data_obito,inicio = c(2015,1), tipo = "mensal")



sinasc_modelo.ajustado <- function(dados,
                                   intervention1 = "2017-03-01",
                                   intervention2 = "2021-03-01"){
  n <- c()
  st <- data.frame(DATA=seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),
                                 to = as.Date(max(dados_sinasc_intervencao$data_variable))-months(1),
                                 by = "1 month"))
  n <- nrow(st)
  if(is.na(intervention2)){
    #Data das intervenções
    intervention1_date = as.Date(intervention1)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()

    typeInterventions <- c("polynomialTrend",
                           "polynomialTrend")
    degree <- c(1,1)

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1),
                                 n=n,
                                 degree=c(1,1))

    # Ajustando o modelo
    fit_lm <- lm(dados~x)
    ResultingTrends <- resultingTrends(n=n,fit_lm,typeInterventions,degree)}

  else{

    #Data das intervenções
    intervention1_date = as.Date(intervention1)
    intervention2_date = as.Date(intervention2)

    tIntervention1     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention1_date,
                                   by = "1 month") %>% length()
    tIntervention2     <- seq.Date(from = as.Date(min(dados_sinasc_intervencao$data_variable)),to = intervention2_date,
                                   by = "1 month") %>% length()

    typeInterventions <- c("polynomialTrend",
                           "polynomialTrend",
                           "polynomialTrend")
    degree <- c(1,1,1)

    x <- interventionModelMatrix(typeInterventions=c("polynomialTrend",
                                                     "polynomialTrend",
                                                     "polynomialTrend"),
                                 tInterventions=c(1,tIntervention1,tIntervention2),
                                 n=n,
                                 degree=c(1,1,1))

    # Ajustando o modelo
    fit_lm <- lm(dados~x)
    ResultingTrends <- resultingTrends(n=n,fit_lm,typeInterventions,degree)
  }

  return(list(ResultingTrends=ResultingTrends, fit_lm=fit_lm))
}


#========================================================================
# Functions for Intervention Analysis - Segmented Models
#========================================================================

#------------------------------------------------------------------------
# To estimate beta2 - abrupt average level change in some tIntervention
#------------------------------------------------------------------------
abruptChange    <- function(tIntervention,
                            n)
  rep(c(0,1),c(tIntervention,n-tIntervention))
#------------------------------------------------------------------------
# To estimate beta3 - trend change after some tIntervention
#------------------------------------------------------------------------
trendChange     <- function(tIntervention,
                            n)
  c(rep(0,tIntervention),1:(n-tIntervention))

#------------------------------------------------------------------------
# To estimate beta1 - trend before intervention (usually begin=1) but can be
# used to estimate polynomial trends after an intervention, case begin==tIntervention
#------------------------------------------------------------------------
polynomialTrend <- function(degree=1,
                            begin=1,
                            tIntervention,
                            n) {
  if(begin==1) {
    c(begin:n)^degree
  } else {
    c(rep(0,begin),(1:(n-begin))^degree)
  }
}

# polynomialTrend(n=10,begin=1,degree = 1)

#------------------------------------------------------------------------
# To create the model matrix including latent variables according the
# interventions/parameters to be inserted/estimated
#------------------------------------------------------------------------
interventionModelMatrix <- function(typeInterventions,
                                    tInterventions,
                                    n,
                                    degree=1) {

  nInterventions <- length(tInterventions)

  #Please provide all typeInterventions and tInterventions
  stopifnot("Please provide all typeInterventions and tInterventions"= length(typeInterventions)==nInterventions,
            length(tInterventions)==nInterventions)



  x <- c()

  for(i in 1:nInterventions) {
    if (typeInterventions[i]== "abrupt"){
      x <- cbind(x,abruptChange(tIntervention=tInterventions[i],
                                n=n))
    }
    if (typeInterventions[i]== "trendChange"){
      x <- cbind(x,trendChange(tIntervention=tInterventions[i],
                               n=n))
    }
    if (typeInterventions[i]== "polynomialTrend"){
      x <- cbind(x,polynomialTrend(begin = tInterventions[i],
                                   n=n,
                                   degree=degree[i]))
    }
  }
  return(x=x)
}

#------------------------------------------------------------------------
# To plot the time series, the estimated model and the interventions-xts package
#------------------------------------------------------------------------
plotInterventions.xts <-   function(fit_lm,
                                    beginDate="2008-01-01",
                                    endDate="2018-10-01",
                                    byStep= "1 months",
                                    tInterventionsPlot,
                                    title = "Time Series Intervention",
                                    legendLabels = c("Observed", "Estimated Model"),
                                    interventionsLabels=c("Intervention")
) {

  par(mfrow=c(1,1))

  time <- seq(as.Date(beginDate),
              to = as.Date(endDate),
              by = byStep)

  print(plot(xts::xts(serie,
                      order.by = time),
             main=title))

  print(xts::addEventLines(xts::xts(interventionsLabels,
                                    time[tInterventionsPlot]), #as.Date(c("2012-01-01"))
                           offset=.6,
                           pos=2,
                           srt=90,
                           cex=1.2,
                           col="red"))

  print(lines(xts::xts(fitted(fit_lm),
                       order.by = time),
              col = 5,
              lty = 4,
              lwd = 3))

  print(xts::addLegend("topleft",
                       legend.names = legendLabels,
                       lty=c(1, 4),
                       lwd = c(1,3),
                       cex = 1.2,
                       col=c(1,5)))
}


#------------------------------------------------------------------------
# To Generate a table with resulting trend information
#------------------------------------------------------------------------

resultingTrends <- function(n,fit_lm,typeInterventions,degree) {

  stopifnot("Please provide an objet of class lm"= class(fit_lm)=="lm")

  coefTrend <- coef(summary(fit_lm))

  # Removing the intercept to compute just resulting trends
  if(sum(stringr::str_detect("Intercept", rownames(coefTrend))>0))
    coefTrend <- coefTrend[-which(stringr::str_detect("Intercept", rownames(coefTrend))),]

  rownames(coefTrend) <- paste0(typeInterventions,"Grau",degree)
  # Removing abrupt changes to compute just resulting trends
  if(sum(stringr::str_detect("abrupt", typeInterventions)>0))
    coefTrend <- coefTrend[-which(stringr::str_detect("abrupt", typeInterventions)),]

  # Removing t statistics
  trends <- coefTrend[,-3]
  colnames(trends) <- c("Estimate","SE","p-value")

  for(i in 1:(nrow(coefTrend)-1)){
    #Estimated trends
    trends[i+1,1] <- coefTrend[i,1]+coefTrend[i+1,1]
    #SE
    trends[i+1,2] <- sqrt(coefTrend[i,2]^2+coefTrend[i+1,2]^2)
  }

  t= trends[,1]/trends[,2]
  trends[,3] = 2* pt(abs(t),df=(n-length(typeInterventions)),lower.tail = F)
  # trends <- round(trends,5)
  return(trends=trends)
}

empty_plot <- function(title = NULL){
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::config(
      displayModeBar = FALSE
    ) %>%
    plotly::layout(
      title = list(
        text = title,
        yref = "paper",
        y = 0.5
      )
    )
}


# Funcao para colocar os títulos na aba análise de intervencao-----

titulo_box <- function(data1,data2,nivel_geografico_nome,escolha_usuario,texto){
  ifelse(
  is.null(data1), texto,
  ifelse(
    is.na(data2),
    paste(
      texto, nivel_geografico_nome,
      "-",escolha_usuario,
      ". Data da intervenção 1:", format(data1,"%b/%Y")),
    paste(
      texto, nivel_geografico_nome,
      "-",escolha_usuario,
      ". Data da intervenção 1:", format(data1,"%b/%Y"),
      ". Data da intervenção 2:", format(data2,"%b/%Y"))))}

