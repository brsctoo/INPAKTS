## Code to prepare `dados_sim_neonatal_intervention` dataset goes here

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
  dplyr::mutate(
    data_categorica = marcar_periodo_intervencao(data_obito),
    municipio_obito = tolower(municipio_obito),
    raca_cor = recodifica_raca_cor(raca_cor, incluir_nao_informado = TRUE),
    tipo_mortalidade = dplyr::case_when(
      tipo_idade == "N.I" ~ "fetal",
      tipo_idade == "Horas" | (tipo_idade == "Dias" & idade <= 6) ~ "neonatal_precoce",
      tipo_idade == "Dias" & idade >= 7 & idade <= 27             ~ "neonatal_tardia",
      TRUE  ~ "outra"
    )
  ) %>%
  padroniza_ignorado(c(
    "tipo_morte_parto",
    "morte_puerperio",
    "escolaridade_mae",
    "estado_civil")
  ) %>%
  dplyr::mutate_at(c("tipo_gestacao","tipo_parto","tipo_obito","tp_morte_ocorreu",
                     "tipo_morte_parto","causa_basica","local_ocorrencia",
                     "data_categorica", "morte_puerperio", "morte_mulher",
                     "escolaridade_mae", "raca_cor", "estado_civil",
                     "local_ocorrencia"),as.factor) %>%
  dplyr::rename(data_variable = data_obito) %>%
  dplyr::select(municipio_obito, data_variable, sexo, raca_cor, tipo_mortalidade)
  # dplyr::select(municipio_obito, data_variable, sexo, raca_cor, tipo_obito, tipo_idade, idade,tipo_mortalidade)

# idade <- dados_sim_neonatal_intervention %>%
# dplyr::select(tipo_idade, tipo_obito, idade,tipo_mortalidade)


# Adicionando macro e  microregião ao dataset usando o dataset do SINASC
dados_sim_neonatal_intervention <- juntar_geo_sinasc(
  dados_sim_neonatal_intervention,
  "municipio_obito",
  dados_sinasc,
  incluir_municipio_oficial = TRUE
)


usethis::use_data(dados_sim_neonatal_intervention, overwrite = TRUE)
