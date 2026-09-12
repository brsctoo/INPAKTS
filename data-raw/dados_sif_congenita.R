## Code to prepare `dados_sif_congenita` dataset goes here

# Sifilis congenita
sif_congenita <- foreign::read.dbf(file = "data/SIFICNET.DBF")%>%
  dplyr::select(NU_NOTIFIC, DT_NOTIFIC, ID_MUNICIP, ID_REGIONA, ID_UNIDADE,
                DT_DIAG, SEM_DIAG, DT_NASC, CS_SEXO, CS_GESTANT, CS_RACA,
                CS_ESCOL_N, EVO_DIAG_N, ANTSIFIL_N)

load("data/munic.RData")
names(munic) <- c("ibge_estabelecimento","municipio","micro","macro","municipio_semacento","populacao")

munic$ibge_estabelecimento <- as.numeric(munic$ibge_estabelecimento)


dados_sif_congenita <- sif_congenita %>% tidyr::drop_na(ID_MUNICIP) %>%
  dplyr::mutate_at("ID_MUNICIP",as.character) %>%
  dplyr::mutate_at("ID_MUNICIP",as.numeric) %>%
  # Mantendo apenas cidades do estado do PR (iniciando com 41:
  dplyr::filter(substr(ID_MUNICIP,1,2) == "41") %>%
  dplyr::rename(ibge_estabelecimento="ID_MUNICIP") %>%
  # dplyr::filter(DT_NOTIFIC >= "2017-01-01") %>%
  dplyr::mutate(data_categorica = ifelse(DT_NOTIFIC > "2020-03-20",'Depois','Antes'),
                data_variable = DT_NOTIFIC) %>%
  dplyr::left_join(munic,by = "ibge_estabelecimento") %>%
  dplyr::mutate_at(c("municipio","micro","macro","municipio_semacento","populacao"), as.factor) %>%
  dplyr::mutate_at(c("CS_RACA", "EVO_DIAG_N"), as.character) %>%
  dplyr::mutate_at(c("CS_RACA", "EVO_DIAG_N"), as.numeric) %>%
  dplyr::mutate(
    # Calcula a idade
    idade = as.numeric(difftime(as.Date(DT_NOTIFIC),as.Date(DT_NASC), units = "days")),

    # Recodifica a idade
    idade1 = dplyr::case_when(
        is.na(idade) ~ "Ignorado",
        idade < 7 ~ "< 7 dias",
        idade >= 7 & idade <= 27 ~ "7 - 27 dias",
        idade >= 28 & idade <= 365 ~ "28 dias - 1 ano",
        idade > 365 ~ "> 1 ano",
        TRUE ~ "Ignorado"
    ),

    # Recodifica Raça/Cor
    CS_RACA = as.numeric(as.character(CS_RACA)),
    CS_RACA = dplyr::case_when(
        is.na(CS_RACA) ~ "Ignorado",
        CS_RACA == 1 ~ "Branca",
        CS_RACA > 1 & CS_RACA < 6  ~ "Não-branca",
        CS_RACA == 9 ~ "Ignorado",
        TRUE ~ NA_character_
    ),

    # Recodifica Evolução
    EVO_DIAG_N = dplyr::case_when(
        is.na(EVO_DIAG_N) ~ "Ignorado",
        EVO_DIAG_N == 1 ~ "Sífilis congênita recente",
        EVO_DIAG_N == 2 ~ "Sífilis congênita tardia",
        EVO_DIAG_N %in% c(3, 4) ~ "Natimorto ou aborto",
        EVO_DIAG_N == 5 ~ "Ignorado",
        TRUE ~ NA_character_
    ),

    # Recodifica Momento ou Diagnóstico Materno
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
    idade1 = forcats::fct_relevel(idade1, "< 7 dias", "7 - 27 dias", "28 dias - 1 ano", "> 1 ano"),
    EVO_DIAG_N = forcats::fct_relevel(EVO_DIAG_N, "Sífilis congênita recente", "Sífilis congênita tardia", "Natimorto ou aborto", "Ignorado"),
    ANTSIFIL_N = forcats::fct_relevel(ANTSIFIL_N, "Durante pré-natal", "Durante parto/curetagem", "Pós parto", "Não realizado", "Ignorado")]
  )%>%
  dplyr::mutate_at(c("CS_RACA","idade1","EVO_DIAG_N","ANTSIFIL_N"), as.factor)

levels(dados_sif_congenita$ANTSIFIL_N)

# summary(sifilis1$ID_MUNICIP)
# sifilis1[sifilis1$NU_NOTIFIC=="5292687",]
# sifilis1[sifilis1$NU_NOTIFIC=="6461456",]

# forcats::fct_count(dados_sif_congenita$ANTSIFIL_N, prop = T)

usethis::use_data(dados_sif_congenita, overwrite = TRUE)
