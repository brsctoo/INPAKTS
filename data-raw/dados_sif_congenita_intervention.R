## Code to prepare `dados_sif_congenita` dataset goes here

# Sifilis congenita
sif_congenita <- foreign::read.dbf(file = "data/SIFICNET.DBF")%>%
  dplyr::select(NU_NOTIFIC, DT_NOTIFIC, ID_MUNICIP, ID_REGIONA, ID_UNIDADE,
                DT_DIAG, SEM_DIAG, DT_NASC, CS_SEXO, CS_GESTANT, CS_RACA,
                CS_ESCOL_N, EVO_DIAG_N, ANTSIFIL_N)

munic <- carregar_munic()

dados_sif_congenita_intervention <- sif_congenita %>% tidyr::drop_na(ID_MUNICIP) %>%
  dplyr::mutate_at("ID_MUNICIP",as.character) %>%
  dplyr::mutate_at("ID_MUNICIP",as.numeric) %>%

  # Mantendo apenas cidades do estado do PR - iniciando com 41:
  dplyr::filter(substr(ID_MUNICIP,1,2) == "41") %>%
  dplyr::rename(ibge_estabelecimento="ID_MUNICIP") %>%

  # dplyr::filter(DT_NOTIFIC >= "2017-01-01") %>%
  dplyr::mutate(data_categorica = marcar_periodo_intervencao(DT_NOTIFIC),
                data_variable = DT_NOTIFIC) %>%
  dplyr::left_join(munic,by = "ibge_estabelecimento") %>%
  dplyr::mutate_at(c("municipio","micro","macro","municipio_semacento","populacao"), as.factor) %>%
  dplyr::mutate_at(c("CS_RACA", "EVO_DIAG_N"), as.character) %>%
  dplyr::mutate_at(c("CS_RACA", "EVO_DIAG_N"), as.numeric) %>%
  dplyr::mutate(
    # Calcula a idade numérica em dias
    idade1 = as.numeric(difftime(as.Date(DT_NOTIFIC), as.Date(DT_NASC), units = "days")),

    # Recodifica a idade
    idade = dplyr::case_when(
      idade1 < 7 ~ "Menos de 7 dias",
      idade1 >= 7 & idade1 <= 27 ~ "7 a 27 dias",
      idade1 >= 28 & idade1 <= 365 ~ "28 dias a 1 ano",
      TRUE ~ NA_character_
    ),

    # Recodifica Raça/Cor
    CS_RACA = recodifica_raca_dbf(CS_RACA),

    # Recodifica Evolução do Diagnóstico
    EVO_DIAG_N = dplyr::case_when(
      is.na(EVO_DIAG_N) ~ "Ignorado",
      EVO_DIAG_N == 1 ~ "Sífilis congênita recente",
      EVO_DIAG_N == 2 ~ "Sífilis congênita tardia",
      EVO_DIAG_N %in% c(3, 4) ~ "Natimorto ou aborto",
      EVO_DIAG_N == 5 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # Recodifica Momento do Diagnóstico Materno
    ANTSIFIL_N = dplyr::case_when(
      is.na(ANTSIFIL_N) ~ "Ignorado",
      ANTSIFIL_N == 1  ~ "Durante pré-natal",
      ANTSIFIL_N == 2  ~ "Durante parto/curetagem",
      ANTSIFIL_N == 3  ~ "Pós parto",
      ANTSIFIL_N == 4  ~ "Não realizado",
      ANTSIFIL_N == 9  ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    CS_RACA = forcats::fct_relevel(CS_RACA, "Branca", "Não-branca", "Ignorado"),
    EVO_DIAG_N = forcats::fct_relevel(EVO_DIAG_N,  "Sífilis congênita recente", "Sífilis congênita tardia", "Natimorto ou aborto", "Ignorado"),
    ANTSIFIL_N = forcats::fct_relevel(ANTSIFIL_N, "Durante pré-natal", "Durante parto/curetagem", "Pós parto", "Não realizado", "Ignorado"),
    raca_cor = forcats::fct_recode(
      CS_RACA,
      "Branca" = "Branca",
      "Não branca" = "Não-branca", # Tira o hífen
      "Não informado" = "Ignorado" # Muda a nomenclatura
    )
  ) %>%
  dplyr::mutate_at(c("CS_RACA","idade","EVO_DIAG_N","ANTSIFIL_N"), as.factor)%>%
  dplyr::relocate(municipio,micro,macro,municipio_semacento) %>%
  dplyr::select(municipio,micro,macro,populacao,data_variable,data_categorica,idade,raca_cor)

# levels(dados_sif_congenita$ANTSIFIL_N)

# summary(sifilis1$ID_MUNICIP)
# sifilis1[sifilis1$NU_NOTIFIC=="5292687",]
# sifilis1[sifilis1$NU_NOTIFIC=="6461456",]

# forcats::fct_count(dados_sif_congenita_intervention$idade, prop = T)

usethis::use_data(dados_sif_congenita_intervention, overwrite = TRUE)
