## code to prepare `dados_map` dataset goes here

data_prep_geo <- function(data_prep,
                          data_primeira_intervencao = "2015-05-01"){

  #Datas somente considerando o mês de janeiro
  datas <- seq.Date(
    from = as.Date(data_primeira_intervencao),
    #from = as.Date(min(data_prep$data_variable)) + months(5),
    to =   as.Date(max(data_prep$data_variable)) - months(1),
    by = "1 month")

  data_inicio_Ano <- as.numeric(format(as.Date(min(data_prep$data_variable)),format = "%Y"))
  data_inicio_Mes <- as.numeric(format(as.Date(min(data_prep$data_variable)),format = "%m"))

  municipio <- c()
  trendAntes <- c()
  trendChange <- c()
  trendChangeCat <- c()
  micro <- c()
  macro <- c()

  df <- data.frame(municipio = levels(data_prep$municipio))
  #87
  for (j in 1:87){
    #i in 1:399
    for (i in 1:length(levels(data_prep$municipio))) {

      municipio[i] <- levels(data_prep$municipio)[i]
      dados <- data_prep[data_prep$municipio == municipio[i], ]
      serie <- return_ts(dados,data_variable , inicio = c(data_inicio_Ano, data_inicio_Mes))
      trendAntes[i] = sinasc_modelo.ajustado(dados = serie, intervention1 = datas[j], intervention2 = NA)$ResultingTrends[1, 1]
      trendChange[i] = sinasc_modelo.ajustado(dados = serie, intervention1 = datas[j], intervention2 = NA)$fit_lm$coefficients[3]
      trendChangeCat[i] = c(ifelse(summary(sinasc_modelo.ajustado(serie)$fit_lm)$coefficients[3,4]<0.05,
                                   ifelse(trendChange[i]>0, "Aumentou", "Diminuiu"), "Estável"))

    }

    df[,j+1] <- as.factor(trendChangeCat)

    # Renomeando o nome da coluna para a data de intervenção
    names(df)[j+1] <- as.character(datas[j])

  }

  load("data/munic.RData")
  munic <- munic %>%
    dplyr::select("MUNICIPIO","RS","MACRO") %>%
    dplyr::rename(municipio= MUNICIPIO, micro = RS, macro = MACRO)

  #Adicionando micro, macro regiões
  df <- df %>%
    dplyr::left_join(munic, by = "municipio")
}



data_prep_geo_rs <- function(data_prep,
                             data_primeira_intervencao = "2015-05-01"){

  #Datas somente considerando o mês de janeiro
  datas <- seq.Date(
    from = as.Date(data_primeira_intervencao),
    #from = as.Date(min(data_prep$data_variable)) + months(5),
    to =   as.Date(max(data_prep$data_variable)) - months(1),
    by = "1 month")

  data_inicio_Ano <- as.numeric(format(as.Date(min(data_prep$data_variable)),format = "%Y"))
  data_inicio_Mes <- as.numeric(format(as.Date(min(data_prep$data_variable)),format = "%m"))

  micro <- c()
  trendAntes <- c()
  trendChange <- c()
  trendChangeCat <- c()
  micro <- c()
  macro <- c()

  df <- data.frame(micro = levels(data_prep$micro))
  #87
  for (j in 1:10){
    #i in 1:399
    for (i in 1:length(levels(data_prep$micro))) {

      micro[i] <- levels(data_prep$micro)[i]
      dados <- data_prep[data_prep$micro == micro[i], ]
      serie <- return_ts(dados,data_variable , inicio = c(data_inicio_Ano, data_inicio_Mes))
      trendAntes[i] = sinasc_modelo.ajustado(dados = serie, intervention1 = datas[j], intervention2 = NA)$ResultingTrends[1, 1]
      trendChange[i] = sinasc_modelo.ajustado(dados = serie, intervention1 = datas[j], intervention2 = NA)$fit_lm$coefficients[3]
      trendChangeCat[i] = c(ifelse(summary(sinasc_modelo.ajustado(serie)$fit_lm)$coefficients[3,4]<0.05,
                                   ifelse(trendChange[i]>0, "Aumentou", "Diminuiu"), "Estável"))

    }

    df[,j+1] <- as.factor(trendChangeCat)

    # Renomeando o nome da coluna para a data de intervenção
    names(df)[j+1] <- as.character(datas[j])

  }

  return(df)
}



# Para os municípios

dados_map_sinasc <- data_prep_geo(dados_sinasc_intervencao)
dados_map_sim_materno <- data_prep_geo(dados_sim_materno_intervention)
dados_map_sim_neonatal <- data_prep_geo(dados_sim_neonatal_intervention)
dados_map_sif_gestante <- data_prep_geo(dados_sif_gestante_intervention)
dados_map_sif_congenita <- data_prep_geo(dados_sif_congenita_intervention)


usethis::use_data(dados_map_sinasc, overwrite = TRUE)

usethis::use_data(dados_map_sim_materno, overwrite = TRUE)

usethis::use_data(dados_map_sim_neonatal, overwrite = TRUE)

usethis::use_data(dados_map_sif_gestante, overwrite = TRUE)

usethis::use_data(dados_map_sif_congenita, overwrite = TRUE)


# Para regionais de saúde
dados_map_sinasc_rs <- data_prep_geo_rs(dados_sinasc_intervencao)
names(dados_map_sinasc_rs)[1] <- c("NUMEROREGSAUDE")

dados_map_sinasc_rs <- dados_map_sinasc_rs %>%
  merge(dengueControl::pr_mun[,c("NUMEROREGSAUDE","nome")]) %>%
  dplyr::rename("micro" = "NUMEROREGSAUDE", "municipio" = "nome")


dados_map_sim_materno_rs <- data_prep_geo_rs(dados_sim_materno_intervention)
names(dados_map_sim_materno_rs)[1] <- c("NUMEROREGSAUDE")

dados_map_sim_materno_rs <-  dados_map_sim_materno_rs %>%
  merge(dengueControl::pr_mun[,c("NUMEROREGSAUDE","nome")]) %>%
  dplyr::rename("micro" = "NUMEROREGSAUDE", "municipio" = "nome")

dados_map_sim_neonatal_rs <- data_prep_geo_rs(dados_sim_neonatal_intervention)
names(dados_map_sim_neonatal_rs)[1] <- c("NUMEROREGSAUDE")

dados_map_sim_neonatal_rs <- dados_map_sim_neonatal_rs %>%
  merge(dengueControl::pr_mun[,c("NUMEROREGSAUDE","nome")]) %>%
  dplyr::rename("micro" = "NUMEROREGSAUDE", "municipio" = "nome")

dados_map_sif_gestante_rs <- data_prep_geo_rs(dados_sif_gestante_intervention)
names(dados_map_sif_gestante_rs)[1] <- c("NUMEROREGSAUDE")

dados_map_sif_gestante_rs <- dados_map_sif_gestante_rs %>%
  merge(dengueControl::pr_mun[,c("NUMEROREGSAUDE","nome")]) %>%
  dplyr::rename("micro" = "NUMEROREGSAUDE", "municipio" = "nome")

dados_map_sif_congenita_rs  <- data_prep_geo_rs(dados_sif_congenita_intervention)
names(dados_map_sif_congenita_rs)[1] <- c("NUMEROREGSAUDE")

dados_map_sif_congenita_rs <- dados_map_sif_congenita_rs %>%
  merge(dengueControl::pr_mun[,c("NUMEROREGSAUDE","nome")]) %>%
  dplyr::rename("micro" = "NUMEROREGSAUDE", "municipio" = "nome")


usethis::use_data(dados_map_sinasc_rs, overwrite = TRUE)

usethis::use_data(dados_map_sim_materno_rs, overwrite = TRUE)

usethis::use_data(dados_map_sim_neonatal_rs, overwrite = TRUE)

usethis::use_data(dados_map_sif_gestante_rs, overwrite = TRUE)

usethis::use_data(dados_map_sif_congenita_rs, overwrite = TRUE)

