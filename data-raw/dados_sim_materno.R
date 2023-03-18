## code to prepare `dados_sim_materno` dataset goes here

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
  dplyr::mutate(data_categorica = ifelse(data_obito > "2020-03-20",'Depois','Antes'),
                data_variable = data_obito,
                tipo_morte_parto = forcats::fct_recode(tipo_morte_parto,
                                                       "N.I."="Ignorado"),
                morte_puerperio = forcats::fct_recode(morte_puerperio,
                                                      "N.I."="Ignorado"),
                escolaridade = forcats::fct_recode(escolaridade,
                                                   "N.I."="Ignorado"),
                estado_civil = forcats::fct_recode(estado_civil,
                                                   "N.I."="Ignorado"),
                municipio_obito = tolower(municipio_obito)) %>%
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


geo_sinasc <- data.frame(micro = dados_sinasc$micro,
                         macro = dados_sinasc$macro,
                         municipio_obito = dados_sinasc$municipio_semacento) %>%
  dplyr::mutate(municipio_obito = tolower(municipio_obito))

munic_sim_materno <- data.frame(
  municipio_obito = dados_sim_materno$municipio_obito)

rm(list=setdiff(ls(), c("dados_sim_materno","geo_sinasc","munic_sim_materno")))

geo <- geo_sinasc %>% dplyr::semi_join(munic_sim_materno) %>%
  dplyr::distinct()


rm(list=setdiff(ls(), c("dados_sim_materno","geo")))

dados_sim_materno <- dados_sim_materno %>%
  dplyr::left_join(geo) %>%
  dplyr::relocate(municipio_obito,micro,macro) %>%
  dplyr::rename(municipio =municipio_obito)

usethis::use_data(dados_sim_materno, overwrite = TRUE)
