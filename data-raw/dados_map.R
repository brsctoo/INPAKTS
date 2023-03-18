## code to prepare `dados_map` dataset goes here

return_map <- function(data_prep, var_date) {
  municipio <- c()
  trendAntes <- c()
  trendChange <- c()
  trendChangeCat <- c()
  for (i in 1:399) {
    municipio[i] <- levels(data_prep$municipio)[i]
    dados <- data_prep[data_prep$municipio == municipio[i], ]
    serie <- return_ts(dados, {{var_date}}, inicio = c(2015, 1))
    trendAntes[i] = sinasc_modelo.ajustado(serie)$ResultingTrends[1, 1]
    trendChange[i] = sinasc_modelo.ajustado(serie)$fit_lm$coefficients[3]
    trendChangeCat[i] = c(ifelse(summary(sinasc_modelo.ajustado(serie)$fit_lm)$coefficients[3,4]<0.05,
                                 ifelse(trendChange[i]>0, "Aumentou", "Diminuiu"), "Estável"))
  }
  dados_map <- data.frame(municipio = municipio,
                          trendAntes = trendAntes,
                          trendChange = trendChange,
                          trendChangeCat = as.factor(trendChangeCat))

  munic <- munic %>% dplyr::rename(municipio = "MUNICIPIO")
  dados_map <-
    dados_map %>% dplyr::left_join(munic, by = "municipio") %>%
    dplyr::rename(micro = RS, macro = MACRO)

  return(dados_map)

}


dados_map_sinasc <- return_map(dados_sinasc_intervencao, data_variable)
dados_map_sim_materno <- return_map(dados_sim_materno_intervention, data_variable)
dados_map_sim_neonatal <- return_map(dados_sim_neonatal_intervention, data_variable)
dados_map_sif_gestante <- return_map(dados_sif_gestante_intervention, data_variable)


usethis::use_data(dados_map_sinasc, overwrite = TRUE)

usethis::use_data(dados_map_sim_materno, overwrite = TRUE)

usethis::use_data(dados_map_sim_neonatal, overwrite = TRUE)

usethis::use_data(dados_map_sif_gestante, overwrite = TRUE)
