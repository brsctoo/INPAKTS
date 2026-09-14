## Code to prepare `dados_sim_materno_intervention` dataset goes here

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
  dplyr::mutate(data_categorica = marcar_periodo_intervencao(data_obito),
                municipio_obito = tolower(municipio_obito),
                raca_cor = recodifica_raca_cor(raca_cor)) %>%
  padroniza_ignorado(c(
    "tipo_morte_parto",
    "morte_puerperio",
    "escolaridade",
    "estado_civil")
  ) %>%
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
  dplyr::mutate(
    idade = classifica_faixa_etaria(idade, tipo = "jovem_adulto_idoso")
  ) %>%
  dplyr::rename(data_variable=data_obito) %>%
  dplyr::select(municipio_obito,data_variable,idade,raca_cor)


# Adicionando macro e  microregião ao dataset usando o dataset do SINASC

dados_sim_materno_intervention <- juntar_geo_sinasc(
  dados_sim_materno_intervention,
  "municipio_obito",
  dados_sinasc,
  incluir_municipio_oficial = TRUE
)

# Salvando os dados na pasta data
usethis::use_data(dados_sim_materno_intervention, overwrite = TRUE)
