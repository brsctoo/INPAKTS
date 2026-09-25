## Code to prepare `dados_map` dataset goes here

tictoc::tic("Processamento dos Mapas")
munic_geo <- carregar_munic() %>% dplyr::select(municipio, micro, macro)

# Para os municípios
dados_map_sinasc <- data_prep_geo(dados_sinasc_intervencao, nivel = "municipio") %>%
  dplyr::left_join(munic_geo, by = "municipio")
dados_map_sim_materno <- data_prep_geo(dados_sim_materno_intervention, nivel = "municipio") %>%
  dplyr::left_join(munic_geo, by = "municipio")
dados_map_sim_neonatal <- data_prep_geo(dados_sim_neonatal_intervention, nivel = "municipio") %>%
  dplyr::left_join(munic_geo, by = "municipio")
dados_map_sif_gestante <- data_prep_geo(dados_sif_gestante_intervention, nivel = "municipio") %>%
  dplyr::left_join(munic_geo, by = "municipio")
dados_map_sif_congenita <- data_prep_geo(dados_sif_congenita_intervention, nivel = "municipio") %>%
  dplyr::left_join(munic_geo, by = "municipio")

usethis::use_data(dados_map_sinasc, overwrite = TRUE)
usethis::use_data(dados_map_sim_materno, overwrite = TRUE)
usethis::use_data(dados_map_sim_neonatal, overwrite = TRUE)
usethis::use_data(dados_map_sif_gestante, overwrite = TRUE)
usethis::use_data(dados_map_sif_congenita, overwrite = TRUE)


# Para regionais de saúde
dados_map_sinasc_rs <- data_prep_geo(dados_sinasc_intervencao, nivel = "micro") %>%
  juntar_regiao_saude()
dados_map_sim_materno_rs <- data_prep_geo(dados_sim_materno_intervention, nivel = "micro") %>%
  juntar_regiao_saude()
dados_map_sim_neonatal_rs <- data_prep_geo(dados_sim_neonatal_intervention, nivel = "micro") %>%
  juntar_regiao_saude()
dados_map_sif_gestante_rs <- data_prep_geo(dados_sif_gestante_intervention, nivel = "micro") %>%
  juntar_regiao_saude()
dados_map_sif_congenita_rs <- data_prep_geo(dados_sif_congenita_intervention, nivel = "micro") %>%
  juntar_regiao_saude()

usethis::use_data(dados_map_sinasc_rs, overwrite = TRUE)
usethis::use_data(dados_map_sim_materno_rs, overwrite = TRUE)
usethis::use_data(dados_map_sim_neonatal_rs, overwrite = TRUE)
usethis::use_data(dados_map_sif_gestante_rs, overwrite = TRUE)
usethis::use_data(dados_map_sif_congenita_rs, overwrite = TRUE)

tictoc::toc()
