## Code to prepare `dados_sim_materno` dataset goes here

sim_2015_2018 <- readr::read_delim(unz(description = "data/SIM-2015-2018.zip",
                                       filename = "SIM-2015-2018.csv"),
                                   delim = ";")

sim_2019_2022 <- readr::read_delim(unz(description = "data/SIM-2019-2022.zip",
                                       filename = "SIM-2019-2022.csv"),
                                   delim = ";")

dados_sim_materno <- rbind(sim_2015_2018, sim_2019_2022) %>%
  dplyr::filter(!(tp_morte_ocorreu %in% c("8","9")) & morte_mulher == "No parto" |
                  !(tp_morte_ocorreu %in% c("8","9")) & morte_mulher == "Na gravidez" |
                  tp_morte_ocorreu %in% c("1", "2", "3", "4", "5"),
                uf_obito=="PR") %>%
  dplyr::select(idade,municipio_obito,data_obito,tipo_gestacao,tipo_parto,tipo_obito,tipo_idade,
                tp_morte_ocorreu,tipo_morte_parto,causa_basica,local_ocorrencia
                ,morte_puerperio, morte_mulher, escolaridade, raca_cor, estado_civil) %>%
  dplyr::mutate(data_categorica = marcar_periodo_intervencao(data_obito),
                data_variable = data_obito,
                municipio_obito = tolower(municipio_obito)) %>%
  padroniza_ignorado(c(
    "tipo_morte_parto",
    "morte_puerperio",
    "escolaridade",
    "estado_civil")
  ) %>%
  dplyr::mutate_at(c("tipo_gestacao","tipo_parto","tipo_obito","tp_morte_ocorreu",
                     "tipo_morte_parto","causa_basica","local_ocorrencia",
                     "data_categorica", "morte_puerperio", "morte_mulher",
                     "escolaridade", "raca_cor", "estado_civil", "local_ocorrencia"),as.factor) %>%
  dplyr::mutate(tp_morte_ocorreu= forcats::fct_recode(tp_morte_ocorreu,
                                                      "Gravidez"="1", "Parto"="2", "Aborto"="3",
                                                      "Até 42 dias pós parto"="4","43 a 365 dias pós parto"="5"))%>%
  dplyr::filter(tipo_idade =="Anos") %>%
  dplyr::mutate_at(c("idade"),as.numeric)


# Adicionando macro e  microregião ao dataset usando o dataset do SINASC
dados_sim_materno <- juntar_geo_sinasc(dados_sim_materno, "municipio_obito", dados_sinasc) %>%
  dplyr::rename(municipio = municipio_obito)

usethis::use_data(dados_sim_materno, overwrite = TRUE)
