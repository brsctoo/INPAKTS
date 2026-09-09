
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
  dplyr::mutate(idade = as.numeric(difftime(as.Date(DT_NOTIFIC),as.Date(DT_NASC), units = "days")),
                idade1 = ifelse(is.na(idade), "Ignorado",
                                ifelse(idade<7,"< 7 dias",
                                    ifelse(idade>=7 & idade<=27,"7 - 27 dias",
                                           ifelse(idade>=28 & idade<=365, "28 dias - 1 ano",
                                                  ifelse(idade>365, "> 1 ano",
                                                         ifelse(idade>=60|idade<=9, "Ignorado", NA)))))),
                CS_RACA = as.numeric(as.character(CS_RACA)),
                CS_RACA = ifelse(is.na(CS_RACA), "Ignorado",
                                 ifelse(CS_RACA==1,"Branca",
                                        ifelse(CS_RACA>1 & CS_RACA<6, "Não-branca",ifelse(CS_RACA==9, "Ignorado",NA)))),
                EVO_DIAG_N = ifelse(is.na(EVO_DIAG_N), "Ignorado",
                                    ifelse(EVO_DIAG_N==1, "Sífilis congênita recente",
                                           ifelse(EVO_DIAG_N==2, "Sífilis congênita tardia",
                                                  ifelse(EVO_DIAG_N%in%c(3,4), "Natimorto ou aborto",
                                                         ifelse(EVO_DIAG_N==5, "Ignorado",NA))))),
                ANTSIFIL_N = ifelse(is.na(ANTSIFIL_N), "Ignorado",
                                    ifelse(ANTSIFIL_N==1, "Durante pré-natal",
                                           ifelse(ANTSIFIL_N==2, "Durante parto/curetagem",
                                                  ifelse(ANTSIFIL_N==3, "Pós parto",
                                                         ifelse(ANTSIFIL_N==4, "Não realizado",
                                                                ifelse(ANTSIFIL_N==9, "Ignorado",NA)))))),
                CS_RACA = forcats::fct_relevel(CS_RACA, levels = "Branca", "Não-branca", "Ignorado"),
                idade1 = forcats::fct_relevel(idade1, levels = "< 7 dias", "7 - 27 dias", "28 dias - 1 ano", "> 1 ano"),
                EVO_DIAG_N = forcats::fct_relevel(EVO_DIAG_N, levels = "Sífilis congênita recente", "Sífilis congênita tardia",
                                                  "Natimorto ou aborto", "Ignorado"),
                ANTSIFIL_N = forcats::fct_relevel(ANTSIFIL_N, levels = "Durante pré-natal", "Durante parto/curetagem", "Pós parto",
                                                  "Não realizado", "Ignorado"))%>%
  dplyr::mutate_at(c("CS_RACA","idade1","EVO_DIAG_N","ANTSIFIL_N"), as.factor)

levels(dados_sif_congenita$ANTSIFIL_N)

# summary(sifilis1$ID_MUNICIP)
# sifilis1[sifilis1$NU_NOTIFIC=="5292687",]
# sifilis1[sifilis1$NU_NOTIFIC=="6461456",]

# forcats::fct_count(dados_sif_congenita$ANTSIFIL_N, prop = T)

usethis::use_data(dados_sif_congenita, overwrite = TRUE)

