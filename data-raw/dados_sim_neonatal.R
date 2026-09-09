## Code to prepare `dados_sim_neonatal` dataset goes here

sim_2015_2018 <- readr::read_delim(unz(description = "data/SIM-2015-2018.zip",
                                       filename = "SIM-2015-2018.csv"),
                                   delim = ";")

sim_2019_2022 <- readr::read_delim(unz(description = "data/SIM-2019-2022.zip",
                                       filename = "SIM-2019-2022.csv"),
                                   delim = ";")



dados_sim_neonatal <- rbind(sim_2015_2018, sim_2019_2022) %>%
  dplyr::filter(tipo_obito == "Fetal" & tipo_idade %in% c("N.I.") |
                  idade == "0" & tipo_idade %in% c("Anos") | tipo_idade %in% c("Horas","Meses"," Minutos","Dias"),
                uf_obito=="PR") %>%
  dplyr::select(idade,municipio_obito,data_obito,tipo_gestacao,tipo_parto,tipo_obito,tipo_idade,
                tp_morte_ocorreu,tipo_morte_parto,causa_basica,local_ocorrencia
                ,morte_puerperio, morte_mulher, escolaridade_mae, raca_cor, estado_civil) %>%
  dplyr::mutate(data_categorica = ifelse(data_obito > "2020-03-20",'Depois','Antes'),
                data_variable = data_obito,
                tipo_morte_parto = forcats::fct_recode(tipo_morte_parto,
                                                       "N.I."="Ignorado"),
                morte_puerperio = forcats::fct_recode(morte_puerperio,
                                                      "N.I."="Ignorado"),
                escolaridade_mae = forcats::fct_recode(escolaridade_mae,
                                                       "N.I."="Ignorado"),
                estado_civil = forcats::fct_recode(estado_civil,
                                                   "N.I."="Ignorado"),
                tipo_gestacao = forcats::fct_recode(tipo_gestacao,
                                                    "22 a 27"="22 a 27 semanas",
                                                    "28 a 31"="28 a 31 semanas",
                                                    "32 a 36"="32 a 36 semanas",
                                                    "37 a 41"="37 a 41 semanas" ,
                                                    "42 e mais"="42 e + semanas",
                                                    "Menos de 22"="Menos 22 semanas"),
                municipio_obito = tolower(municipio_obito)) %>%
  dplyr::mutate_at(c("tipo_gestacao","tipo_parto","tipo_obito","tp_morte_ocorreu",
                     "tipo_morte_parto","causa_basica","local_ocorrencia",
                     "data_categorica", "morte_puerperio", "morte_mulher",
                     "escolaridade_mae", "raca_cor", "estado_civil", "local_ocorrencia"),as.factor) %>%
  dplyr::mutate_at(c("idade"),as.numeric)

# Adicionando macro e  microregião ao dataset usando o dataset do SINASC

geo_sinasc <- data.frame(micro = dados_sinasc$micro,
                         macro =dados_sinasc$macro,
                         municipio_obito =dados_sinasc$municipio_semacento) %>%
  dplyr::mutate(municipio_obito = tolower(municipio_obito))

munic_sim_neonatal <- data.frame(municipio_obito=dados_sim_neonatal$municipio_obito)

rm(list=setdiff(ls(), c("dados_sim_neonatal","geo_sinasc","munic_sim_neonatal")))

geo <- geo_sinasc %>% dplyr::semi_join(munic_sim_neonatal) %>%
  dplyr::distinct()


rm(list=setdiff(ls(), c("dados_sim_neonatal","geo")))

dados_sim_neonatal <- dados_sim_neonatal %>%
  dplyr::left_join(geo) %>%
  dplyr::relocate(municipio_obito,micro,macro) %>%
  dplyr::rename(municipio =municipio_obito )



usethis::use_data(dados_sim_neonatal, overwrite = TRUE)
