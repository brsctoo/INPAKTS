## code to prepare `dados_sim_neonatal_intervention` dataset goes here


require(magrittr,include.only = "%>%")

sim_2015_2018 <- readr::read_delim(unz(description = "data/SIM-2015-2018.zip",
                                       filename = "SIM-2015-2018.csv"),
                                   delim = ";")

sim_2019_2022 <- readr::read_delim(unz(description = "data/SIM-2019-2022.zip",
                                       filename = "SIM-2019-2022.csv"),
                                   delim = ";")

dados_sim_neonatal_intervention <- rbind(sim_2015_2018, sim_2019_2022) %>%
  dplyr::filter(tipo_obito == "Fetal" & tipo_idade %in% c("N.I.") |
                  idade == "0" & tipo_idade %in% c("Anos") | tipo_idade %in% c("Horas","Meses"," Minutos","Dias"),
                uf_obito=="PR") %>%
  dplyr::select(idade,municipio_obito,data_obito,tipo_gestacao,tipo_parto,tipo_obito,
                tipo_idade, tp_morte_ocorreu,tipo_morte_parto,causa_basica,local_ocorrencia
                ,morte_puerperio, morte_mulher, escolaridade_mae, raca_cor,
                estado_civil,sexo,raca_cor) %>%
  dplyr::mutate_at(c("idade"),as.numeric) %>%
  dplyr::mutate(data_categorica = ifelse(data_obito > "2020-03-20",'Depois','Antes'),
                tipo_morte_parto = forcats::fct_recode(tipo_morte_parto,
                                                       "N.I."="Ignorado"),
                morte_puerperio = forcats::fct_recode(morte_puerperio,
                                                      "N.I."="Ignorado"),
                escolaridade_mae = forcats::fct_recode(escolaridade_mae,
                                                       "N.I."="Ignorado"),
                estado_civil = forcats::fct_recode(estado_civil,
                                                   "N.I."="Ignorado"),
                municipio_obito = tolower(municipio_obito),
                raca_cor = forcats::fct_recode(raca_cor,
                                               "Branca" = "Branca",
                                               "Não branca" = "Preta",
                                               "Não branca" = "Amarela",
                                               "Não branca" = "Indígena",
                                               "Não branca" = "Parda",
                                               "Não informado" = "N.I."),
                tipo_mortalidade = ifelse(tipo_idade == "N.I.", "fetal",
                                          ifelse(tipo_idade == "Horas" , "neonatal_precoce",
                                                 ifelse(tipo_idade  == "Dias"  & idade <=6 , "neonatal_precoce",
                                                        ifelse( tipo_idade == "Dias" & idade>=7 & idade<=27  , "neonatal_tardia", "outra"))))) %>%
  dplyr::mutate_at(c("tipo_gestacao","tipo_parto","tipo_obito","tp_morte_ocorreu",
                     "tipo_morte_parto","causa_basica","local_ocorrencia",
                     "data_categorica", "morte_puerperio", "morte_mulher",
                     "escolaridade_mae", "raca_cor", "estado_civil",
                     "local_ocorrencia"),as.factor) %>%
  dplyr::rename(data_variable = data_obito) %>%
  dplyr::select(municipio_obito, data_variable, sexo, raca_cor, tipo_mortalidade)
  #dplyr::select(municipio_obito, data_variable, sexo, raca_cor, tipo_obito, tipo_idade, idade,tipo_mortalidade)

# idade <- dados_sim_neonatal_intervention %>%
#   dplyr::select(tipo_idade, tipo_obito, idade,tipo_mortalidade)


#Adicionando macro e  microregião ao dataset usando o dataset do SINASC


geo_sinasc <- data.frame(micro = dados_sinasc$micro,
                         macro = dados_sinasc$macro,
                         municipio = dados_sinasc$municipio,
                         municipio_obito = dados_sinasc$municipio_semacento) %>%
  dplyr::mutate(municipio_obito = tolower(municipio_obito))

munic_sim_neonatal <- data.frame(municipio_obito=dados_sim_neonatal_intervention$municipio_obito)

rm(list=setdiff(ls(), c("dados_sim_neonatal_intervention","geo_sinasc","munic_sim_neonatal")))

geo <- geo_sinasc %>% dplyr::semi_join(munic_sim_neonatal) %>%
  dplyr::distinct()


rm(list=setdiff(ls(), c("dados_sim_neonatal_intervention","geo")))

dados_sim_neonatal_intervention <- dados_sim_neonatal_intervention %>%
  dplyr::left_join(geo) %>%
  dplyr::relocate(municipio_obito,micro,macro) %>%
  dplyr::relocate(municipio,micro,macro) %>%
  dplyr::select(-municipio_obito)


usethis::use_data(dados_sim_neonatal_intervention, overwrite = TRUE)

