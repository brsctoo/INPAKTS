## Code to prepare `dados_sinasc` dataset goes here

sinasc_2015 <- readr::read_delim(unz(description = "data/sinasc-2015-2019.zip",
                                     filename = "sinasc-2015.csv"),
                                 delim = ";")
# locale = locale(encoding='UTF-8'))
sinasc_2016 <- readr::read_delim(unz(description = "data/sinasc-2015-2019.zip",
                                     filename = "sinasc-2016.csv"),
                                 delim = ";")
sinasc_2017 <- readr::read_delim(unz(description = "data/sinasc-2015-2019.zip",
                                     filename = "sinasc-2017.csv"),
                                 delim = ";")
sinasc_2018 <- readr::read_delim(unz(description = "data/sinasc-2015-2019.zip",
                                     filename = "sinasc-2018.csv"),
                                 delim = ";")
# sinasc_2019 <- readr::read_delim(unz(description = "data/sinasc-2015-2019.zip",
#                                      filename = "sinasc-2019.csv"),
#                                  delim = ";")
# sinasc_2020_2021 <- readr::read_delim(unz(description = "data/sinasc-2020-2021.zip",
#                                           filename = "sinasc-2020-2021.csv"),
#                                       delim = ";")

sinasc_2019_2022 <- readr::read_delim(unz(description = "data/SINASC-2019-2022.zip",
                                          filename = "SINASC-2019-2022.csv"),
                                      delim = ";")

load("data/munic.RData")
names(munic) <- c("ibge_estabelecimento","municipio","micro","macro","municipio_semacento","populacao")

munic$ibge_estabelecimento <- as.numeric(munic$ibge_estabelecimento)


dados_sinasc <- rbind(sinasc_2015,sinasc_2016,sinasc_2017,sinasc_2018,sinasc_2019_2022) %>%
  dplyr::select(cnes_estabelecimento,ibge_estabelecimento,semanas_de_gestacao,tipo_parto,consulta_prenatal...18,
                data_nascimento,parto_cesarea, semana_gestacao,cesarea_anterior_parto,mes_gestacao_prenatal) %>%
  # Retirando linhas com codigo NA:
  tidyr::drop_na(ibge_estabelecimento) %>%
  # Mantendo apenas cidades do estado do PR (iniciando com 41:
  dplyr::filter(substr(ibge_estabelecimento,1,2) == "41") %>%
  # dplyr::filter(data_nascimento >= "2017-01-01") %>%
  dplyr::mutate(data_categorica = ifelse(data_nascimento > "2020-03-20",'Depois','Antes')) %>%
  dplyr::mutate(data_variable = data_nascimento) %>%
  dplyr::left_join(munic,by = "ibge_estabelecimento") %>%
  dplyr::mutate_at(c("semanas_de_gestacao","tipo_parto","consulta_prenatal...18","parto_cesarea",
                     "cesarea_anterior_parto","macro","municipio","micro"),as.factor) %>%
  dplyr::rename(consulta_prenatal="consulta_prenatal...18") %>%
  dplyr::mutate(
    # Recodificação de Fatores Simples
    consulta_prenatal = forcats::fct_recode(
      consulta_prenatal,
      "Ignorado" = "Não informado"
    ),

    semanas_de_gestacao = forcats::fct_recode(
      semanas_de_gestacao,
      "Menos de 22" = "Menos de 22 semanas",
      "Ignorado" = "Não informado",
      "22 a 27" = "22 a 27 semanas",
      "28 a 31" = "28 a 31 semanas",
      "32 a 36" = "32 a 36 semanas",
      "37 a 41" = "37 a 41 semanas",
      "42 e mais" = "42 semanas e mais"
    ),

    cesarea_anterior_parto = forcats::fct_recode(
      cesarea_anterior_parto,
      "Ignorado" = "Não informado",
      "Ignorado" = "Não se aplica",
      "Ignorado" = "ignorado"
    ),

    parto_cesarea1 = forcats::fct_recode(
      parto_cesarea,
      "0"="Nenhum",
      "4000"= "Não informado"
    ),

    # Tratamento de variável - parto_cesarea
    parto_cesarea1 = as.numeric(as.character(parto_cesarea1)),
    parto_cesarea1 = dplyr::case_when(
      parto_cesarea1 > 36 ~ "Ignorado",
      parto_cesarea1 == 0 ~ "Nenhum",
      parto_cesarea1 == 1 ~ "Um",
      parto_cesarea1 == 2 ~ "Dois",
      parto_cesarea1 > 2 ~ "Mais que dois",
      TRUE ~ NA_character_
    ),

    # Tratamento de variável - mes_gestacao_prenatal
    mes_gestacao_prenatal1 = as.numeric(mes_gestacao_prenatal),
    mes_gestacao_prenatal1 = dplyr::case_when(
      is.na(mes_gestacao_prenatal1) ~ "Ignorado",
      mes_gestacao_prenatal1 >= 1 & mes_gestacao_prenatal1 < 4  ~ "1º Trimestre",
      mes_gestacao_prenatal1 >= 4 & mes_gestacao_prenatal1 < 7  ~ "2º Trimestre",
      mes_gestacao_prenatal1 >= 7 & mes_gestacao_prenatal1 < 11 ~ "3º Trimestre",
      TRUE ~ "Ignorado"
    ),

    # Ordenação de níveis (Fatores)
    consulta_prenatal = forcats::fct_relevel(
      consulta_prenatal,
      "Nenhuma","1 a 3", "4 a 6", "7 e mais", "Ignorado"
    ),

    semanas_de_gestacao = forcats::fct_relevel(
      semanas_de_gestacao,
      "Menos de 22", "22 a 27", "28 a 31", "32 a 36", "37 a 41", "42 e mais", "Ignorado"
    ),

    cesarea_anterior_parto = forcats::fct_relevel(
      cesarea_anterior_parto,
      "Sim", "Não", "Ignorado"
    ),

    parto_cesarea1 = forcats::fct_relevel(
      parto_cesarea1,
      "Nenhum", "Um", "Dois", "Mais que dois",  "Ignorado")
  ) %>%
  dplyr::mutate_at(c("parto_cesarea1","mes_gestacao_prenatal1"),as.factor) %>%
  dplyr::relocate(municipio,micro,macro,municipio_semacento)


usethis::use_data(dados_sinasc, overwrite = TRUE)
#levels(dados_sinasc$parto_cesarea1)
