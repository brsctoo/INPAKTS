## Code to prepare `dados_sinasc_intervencao` dataset goes here

munic <- carregar_munic()

dados_sinasc_intervencao <- carregar_sinasc_bruto() %>%
  dplyr::select(cnes_estabelecimento,ibge_estabelecimento,semanas_de_gestacao,tipo_parto,consulta_prenatal...18,
                data_nascimento,parto_cesarea, semana_gestacao,cesarea_anterior_parto,mes_gestacao_prenatal,
                sexo, idade, raça_cor_rn) %>%
  # Retirando linhas com NA:
  tidyr::drop_na(ibge_estabelecimento) %>%
  # Mantendo apenas cidades do estado do PR (iniciando com 41):
  dplyr::filter(substr(ibge_estabelecimento,1,2) == "41") %>%
  dplyr::mutate(data_categorica = marcar_periodo_intervencao(data_nascimento)) %>%
  dplyr::left_join(munic,by = "ibge_estabelecimento") %>%
  dplyr::mutate_at(c("semanas_de_gestacao","tipo_parto","consulta_prenatal...18","parto_cesarea",
                      "cesarea_anterior_parto","macro","municipio","micro","sexo"),as.factor) %>%
  dplyr::rename(consulta_prenatal="consulta_prenatal...18", raca_cor = raça_cor_rn) %>%
  dplyr::mutate(
    # Recodificação de Fatores
    consulta_prenatal = forcats::fct_recode(
      consulta_prenatal,
      "Ignorado" = "Não informado"
    ),

    semanas_de_gestacao = forcats::fct_recode(
      semanas_de_gestacao,
      "Ignorado" = "Não informado"
    ),

    cesarea_anterior_parto = forcats::fct_recode(
      cesarea_anterior_parto,
      "Ignorado" = "Não informado",
      "Ignorado" = "Não se aplica",
      "Ignorado" = "ignorado"
    ),

    parto_cesarea1 = recodifica_parto_cesarea(parto_cesarea)

    # Tratamento da variável - mes_gestacao_prenatal
    mes_gestacao_prenatal1 = recodifica_mes_gestacao_prenatal(mes_gestacao_prenatal)

    # Classificação por Idade
    idade = classifica_faixa_etaria(idade, tipo = "jovem_adulto_idoso"),

    # Agrupamento por Raça/Cor
    raca_cor = recodifica_raca_cor(raca_cor)
  ) %>%
  tidyr::drop_na(raca_cor) %>%
  dplyr::mutate_at(c("parto_cesarea1","mes_gestacao_prenatal1"),as.factor) %>%
  dplyr::relocate(municipio,micro,macro,municipio_semacento) %>%
  dplyr::rename(data_variable = data_nascimento) %>%
  dplyr::select(municipio,micro,macro,populacao,data_variable,data_categorica,
                sexo,idade,raca_cor)


usethis::use_data(dados_sinasc_intervencao, overwrite = TRUE)
