## Code to prepare `dados_sifilis` dataset goes here

# Sifilis gestante
sif_gestante <- foreign::read.dbf(file = "data/SIFGENET.DBF")%>%
  dplyr::select(NU_NOTIFIC, DT_NOTIFIC, ID_MUNICIP, ID_REGIONA, ID_UNIDADE,
                DT_DIAG, SEM_DIAG, DT_NASC, CS_SEXO, CS_GESTANT, CS_RACA,
                CS_ESCOL_N, TPEVIDENCI, TPTESTE1,TPCONFIRMA)

munic <- carregar_munic()

dados_sif_gestante_intervention <- sif_gestante %>% tidyr::drop_na(ID_MUNICIP) %>%
  dplyr::mutate_at("ID_MUNICIP",as.character) %>%
  dplyr::mutate_at("ID_MUNICIP",as.numeric) %>%

  # Mantendo apenas cidades do estado do PR - iniciando com 41:
  dplyr::filter(substr(ID_MUNICIP,1,2) == "41") %>%
  dplyr::rename(ibge_estabelecimento="ID_MUNICIP") %>%
  dplyr::mutate(data_categorica = ifelse(DT_NOTIFIC > "2020-03-20",'Depois','Antes')) %>%
  dplyr::left_join(munic,by = "ibge_estabelecimento") %>%
  dplyr::mutate_at(c("municipio","micro","macro","municipio_semacento","populacao"), as.factor) %>%

  # dplyr::mutate_at(c("CS_RACA"),as.factor) %>%
  dplyr::mutate_at(c("CS_RACA", "TPEVIDENCI", "CS_ESCOL_N", "TPTESTE1", "TPCONFIRMA"), as.character) %>%
  dplyr::mutate_at(c("CS_RACA", "TPEVIDENCI", "CS_ESCOL_N", "TPTESTE1", "TPCONFIRMA"), as.numeric) %>%
  dplyr::mutate(
    # Recodifica Raça/Cor
    CS_RACA = recodifica_raca_dbf(CS_RACA),

    # Calcula a idade da mãe em anos
    idade_mae = as.numeric(round(difftime(as.Date(DT_NOTIFIC),as.Date(DT_NASC), units = "days") / 365, 0)),

    # Recodifica Tipo de Evidência
    TPEVIDENCI = dplyr::case_when(
      is.na(TPEVIDENCI) ~ "Ignorado",
      TPEVIDENCI == 1 ~ "Primária",
      TPEVIDENCI == 2 ~ "Secundária",
      TPEVIDENCI == 3 ~ "Terciária",
      TPEVIDENCI == 4 ~ "Latente",
      TPEVIDENCI == 9 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # Recodifica Escolaridade
    CS_ESCOL_N = dplyr::case_when(
      is.na(CS_ESCOL_N) ~ "Ignorado",
      CS_ESCOL_N %in% c(1, 2, 3) ~ "EF incompleto",
      CS_ESCOL_N %in% 4:8 ~ "EF completo",
      CS_ESCOL_N %in% c(0, 9, 10) ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # Recodifica Teste 1
    TPTESTE1 = dplyr::case_when(
      is.na(TPTESTE1) ~ "Ignorado",
      TPTESTE1 == 1 ~ "Reagente",
      TPTESTE1 == 2 ~ "Não reagente",
      TPTESTE1 == 3 ~ "Não realizado",
      TPTESTE1 == 9 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # Recodifica Teste Confirmatório
    TPCONFIRMA = dplyr::case_when(
      is.na(TPCONFIRMA) ~ "Ignorado",
      TPCONFIRMA == 1 ~ "Reagente",
      TPCONFIRMA == 2 ~ "Não reagente",
      TPCONFIRMA == 3 ~ "Não realizado",
      TPCONFIRMA == 9 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # Categoriza a Idade da Mãe
    idade = classifica_faixa_etaria(idade_mae, tipo = "jovem_adulto_idoso"),

    # Criação da raca_cor renomeando categorias
    raca_cor = forcats::fct_recode (
      CS_RACA,
      "Branca" = "Branca",
      "Não branca" = "Não-branca",
      "Não informado" = "Ignorado"
    )
  ) %>%
  dplyr::mutate_at(c("TPCONFIRMA"), as.factor)%>%
  dplyr::relocate(municipio,micro,macro,municipio_semacento) %>%
  dplyr::rename(data_variable = DT_NOTIFIC) %>%
  dplyr::select(municipio,micro,macro,populacao,data_variable,data_categorica,idade,raca_cor)



# (length(seq(from = DT_NASC, to = DT_NOTIFIC, by = 'year'))-1))

# forcats::fct_count(dados_sif_gestante$TPCONFIRMA, prop = T)
# summary(sifilis1$ID_MUNICIP)
# sifilis1[sifilis1$NU_NOTIFIC=="5292687",]
# sifilis1[sifilis1$NU_NOTIFIC=="6461456",]

usethis::use_data(dados_sif_gestante_intervention, overwrite = TRUE)
