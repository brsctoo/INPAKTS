## code to prepare `dados_sim_materno_intervention` dataset goes here

require(magrittr,include.only = "%>%")

# Carregando dados dos arquivos .zip
sim_2015_2018 <- readr::read_delim(unz(description = "data/SIM-2015-2018.zip",
                                       filename = "SIM-2015-2018.csv"),
                                   delim = ";")

sim_2019_2022 <- readr::read_delim(unz(description = "data/SIM-2019-2022.zip",
                                       filename = "SIM-2019-2022.csv"),
                                   delim = ";")

# Filtrando os dados
dados_sim_materno_intervention <- rbind(sim_2015_2018, sim_2019_2022) %>%
  dplyr::filter(!(tp_morte_ocorreu %in% c("8","9")) & morte_mulher == "No parto" |
                  !(tp_morte_ocorreu %in% c("8","9")) & morte_mulher == "Na gravidez" |
                  tp_morte_ocorreu %in% c("1", "2", "3", "4", "5"),
                uf_obito=="PR") %>%
  dplyr::select(municipio_obito, data_obito, tipo_gestacao, tipo_parto, tipo_obito,
                tipo_idade, tp_morte_ocorreu, tipo_morte_parto,causa_basica,
                local_ocorrencia, morte_puerperio, morte_mulher, escolaridade,
                raca_cor, estado_civil, idade) %>%
  dplyr::mutate_at(c("idade"),as.numeric) %>%
  dplyr::mutate(data_categorica = ifelse(data_obito > "2020-03-20",'Depois','Antes'),
                tipo_morte_parto = forcats::fct_recode(tipo_morte_parto,
                                                       "N.I."="Ignorado"),
                morte_puerperio = forcats::fct_recode(morte_puerperio,
                                                      "N.I."="Ignorado"),
                escolaridade = forcats::fct_recode(escolaridade,
                                                   "N.I."="Ignorado"),
                estado_civil = forcats::fct_recode(estado_civil,
                                                   "N.I."="Ignorado"),
                municipio_obito = tolower(municipio_obito),
                raca_cor = forcats::fct_recode(raca_cor,
                                               "Branca" = "Branca",
                                               "Não branca" = "Preta",
                                               "Não branca" = "Amarela",
                                               "Não branca" = "Indígena",
                                               "Não branca" = "Parda" )) %>%
  dplyr::mutate_at(c("tipo_gestacao","tipo_parto","tipo_obito","tp_morte_ocorreu",
                     "tipo_morte_parto","causa_basica","local_ocorrencia",
                     "data_categorica", "morte_puerperio", "morte_mulher",
                     "escolaridade", "raca_cor", "estado_civil", "local_ocorrencia"),
                   as.factor) %>%
  dplyr::mutate(tp_morte_ocorreu= forcats::fct_recode(tp_morte_ocorreu,
                                                      "Gravidez"="1",
                                                      "Parto"="2",
                                                      "Aborto"="3",
                                                      "Até 42 dias pós parto"="4",
                                                      "43 a 365 dias pós parto"="5"))%>%
  dplyr::filter(tipo_idade =="Anos") %>%
  dplyr::mutate(idade = ifelse(idade>=10 & idade<19, "Jovens: 10 a 18 anos",
                               ifelse(idade>=19 & idade < 31, "Adultos Jovens: 19 a 30 anos",
                                      ifelse(idade>=31 & idade<=60,"Adultos: 31 a 59 anos",
                                             ifelse(idade > 60 & idade<90, "Idosos: acima de 60", NA))))) %>%
  dplyr::rename(data_variable=data_obito) %>%
  dplyr::select(municipio_obito,data_variable,idade,raca_cor)



# Adicionando macro e  microregião ao dataset usando o dataset do SINASC

geo_sinasc <- data.frame(micro = dados_sinasc$micro,
                         macro = dados_sinasc$macro,
                         municipio = dados_sinasc$municipio,
                         municipio_obito = dados_sinasc$municipio_semacento) %>%
  dplyr::mutate(municipio_obito = tolower(municipio_obito))


munic_sim_materno <- data.frame(
  municipio_obito = dados_sim_materno_intervention$municipio_obito)

rm(list=setdiff(ls(), c("dados_sim_materno_intervention","geo_sinasc","munic_sim_materno")))

geo <- geo_sinasc %>% dplyr::semi_join(munic_sim_materno) %>%
  dplyr::distinct()


rm(list=setdiff(ls(), c("dados_sim_materno_intervention","geo")))

dados_sim_materno_intervention <- dados_sim_materno_intervention %>%
  dplyr::left_join(geo) %>%
  dplyr::relocate(municipio,micro,macro) %>%
  dplyr::select(-municipio_obito)


# Salvando os dados na pasta data
usethis::use_data(dados_sim_materno_intervention, overwrite = TRUE)

