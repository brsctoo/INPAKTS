## Code to prepare `dados_sinasc` dataset goes here

munic <- carregar_munic()

dados_sinasc <- carregar_sinasc_bruto() %>%
  dplyr::select(cnes_estabelecimento,ibge_estabelecimento,semanas_de_gestacao,tipo_parto,consulta_prenatal...18,
                data_nascimento,parto_cesarea, semana_gestacao,cesarea_anterior_parto,mes_gestacao_prenatal) %>%
  # Retirando linhas com codigo NA:
  tidyr::drop_na(ibge_estabelecimento) %>%
  # Mantendo apenas cidades do estado do PR (iniciando com 41:
  dplyr::filter(substr(ibge_estabelecimento,1,2) == "41") %>%
  # dplyr::filter(data_nascimento >= "2017-01-01") %>%
  dplyr::mutate(data_categorica = marcar_periodo_intervencao(data_nascimento)) %>%
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

    parto_cesarea1 = recodifica_parto_cesarea(parto_cesarea),

    # Tratamento de variável - mes_gestacao_prenatal
    mes_gestacao_prenatal1 = recodifica_mes_gestacao_prenatal(mes_gestacao_prenatal),

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
