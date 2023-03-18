
## code to prepare `dados_sifilis` dataset goes here
# Sifilis gestante
sif_gestante <- foreign::read.dbf(file = "data/SIFGENET.DBF")%>%
  dplyr::select(NU_NOTIFIC, DT_NOTIFIC, ID_MUNICIP, ID_REGIONA, ID_UNIDADE,
                DT_DIAG, SEM_DIAG, DT_NASC, CS_SEXO, CS_GESTANT, CS_RACA,
                CS_ESCOL_N, TPEVIDENCI, TPTESTE1,TPCONFIRMA)

load("data/munic.RData")
names(munic) <- c("ibge_estabelecimento","municipio","micro","macro","municipio_semacento","populacao")

munic$ibge_estabelecimento <- as.numeric(munic$ibge_estabelecimento)


dados_sif_gestante <- sif_gestante %>% tidyr::drop_na(ID_MUNICIP) %>%
  dplyr::mutate_at("ID_MUNICIP",as.character) %>%
  dplyr::mutate_at("ID_MUNICIP",as.numeric) %>%
  #Mantendo apenas cidades do estado do PR (iniciando com 41:
  dplyr::filter(substr(ID_MUNICIP,1,2) == "41") %>%
  dplyr::rename(ibge_estabelecimento="ID_MUNICIP") %>%
  dplyr::filter(DT_NOTIFIC >= "2017-01-01") %>%
  dplyr::mutate(data_categorica = ifelse(DT_NOTIFIC > "2020-03-20",'Depois','Antes')) %>%
  dplyr::left_join(munic,by = "ibge_estabelecimento") %>%
  dplyr::mutate_at(c("municipio","micro","macro","municipio_semacento","populacao"), as.factor) %>%
  #dplyr::mutate_at(c("CS_RACA"),as.factor) %>%
  dplyr::mutate_at(c("CS_RACA", "TPEVIDENCI", "CS_ESCOL_N", "TPTESTE1", "TPCONFIRMA"), as.character) %>%
  dplyr::mutate_at(c("CS_RACA", "TPEVIDENCI", "CS_ESCOL_N", "TPTESTE1", "TPCONFIRMA"), as.numeric) %>%
  dplyr::mutate(CS_RACA = as.numeric(as.character(CS_RACA)),
                CS_RACA = ifelse(is.na(CS_RACA), "Ignorado",
                                 ifelse(CS_RACA==1,"Branca",
                                        ifelse(CS_RACA>1 & CS_RACA<6, "Não-branca",ifelse(CS_RACA==9, "Ignorado",NA)))),
                idade_mae = as.numeric(round(difftime(as.Date(DT_NOTIFIC),as.Date(DT_NASC), units = "days")/365,0)),
                idade_mae1 = ifelse(is.na(idade_mae), "Ignorado",
                                 ifelse(idade_mae>=10 & idade_mae<=14,"10-14",
                                        ifelse(idade_mae>=15 & idade_mae<=19, "15-19",
                                               ifelse(idade_mae>=20 & idade_mae<=39, "20-39",
                                                      ifelse(idade_mae>=40 & idade_mae<=59,"40-59",
                                                             ifelse(idade_mae>=60|idade_mae<=9, "Ignorado", NA)))))),
                TPEVIDENCI = ifelse(is.na(TPEVIDENCI), "Ignorado",
                                    ifelse(TPEVIDENCI==1, "Primária",
                                           ifelse(TPEVIDENCI==2, "Secundária",
                                                  ifelse(TPEVIDENCI==3, "Terciária",
                                                         ifelse(TPEVIDENCI==4, "Latente",
                                                                ifelse(TPEVIDENCI==9, "Ignorado",NA)))))),
                CS_ESCOL_N = ifelse(is.na(CS_ESCOL_N), "Ignorado",
                                    ifelse(CS_ESCOL_N%in%c(1,2,3), "EF incompleto",
                                           ifelse(CS_ESCOL_N%in%c(4,5,6,7,8), "EF completo",
                                                  ifelse(CS_ESCOL_N%in%c(0,9,10), "Ignorado",NA)))),
                TPTESTE1 = ifelse(is.na(TPTESTE1), "Ignorado",
                                  ifelse(TPTESTE1==1, "Reagente",
                                         ifelse(TPTESTE1==2, "Não reagente",
                                                ifelse(TPTESTE1==3, "Não realizado",
                                                       ifelse(TPTESTE1==9, "Ignorado", NA))))),
                TPCONFIRMA = ifelse(is.na(TPCONFIRMA), "Ignorado",
                                    ifelse(TPCONFIRMA==1, "Reagente",
                                           ifelse(TPCONFIRMA==2, "Não reagente",
                                                  ifelse(TPCONFIRMA==3, "Não realizado",
                                                         ifelse(TPCONFIRMA==9, "Ignorado", NA))))),
                CS_RACA = forcats::fct_relevel(CS_RACA, levels = "Branca", "Não-branca", "Ignorado"),
                TPEVIDENCI = forcats::fct_relevel(TPEVIDENCI, levels = "Primária", "Secundária", "Terciária", "Latente", "Ignorado"),
                TPTESTE1 = forcats::fct_relevel(TPTESTE1, levels = "Reagente", "Não reagente",  "Não realizado","Ignorado"),
                TPCONFIRMA = forcats::fct_relevel(TPCONFIRMA, levels = "Reagente", "Não reagente",  "Não realizado","Ignorado")) %>%
  dplyr::mutate_at(c("CS_RACA","idade_mae1","TPEVIDENCI","CS_ESCOL_N","TPTESTE1","TPCONFIRMA"), as.factor)

#levels(dados_sif_gestante$TPCONFIRMA)

                  #(length(seq(from = DT_NASC, to = DT_NOTIFIC, by = 'year'))-1))

#forcats::fct_count(dados_sif_gestante$TPCONFIRMA, prop = T)
#summary(sifilis1$ID_MUNICIP)
#sifilis1[sifilis1$NU_NOTIFIC=="5292687",]
#sifilis1[sifilis1$NU_NOTIFIC=="6461456",]

usethis::use_data(dados_sif_gestante, overwrite = TRUE)

#
# idade_mae <- round(difftime(as.Date(dados_sif_gestante$DT_NOTIFIC), as.Date(dados_sif_gestante$DT_NASC) , units = "days")/365,0)
#
# CS_RACA = forcats::fct_recode(CS_RACA,
#                               "Branca" = "1",
#                               "Não-branca" = "2",
#                               "Não-branca" = "3",
#                               "Não-branca" = "4",
#                               "Não-branca" = "5",
#                               "Ignorado" = "9"),
