## code to prepare `dados_sinasc_intervencao` dataset goes here

require(magrittr, include.only = "%>%")

sinasc_2015 <- readr::read_delim(unz(description = "data/sinasc-2015-2019.zip",
                                     filename = "sinasc-2015.csv"),
                                 delim = ";")
#locale = locale(encoding='UTF-8'))
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


dados_sinasc_intervencao <- rbind(sinasc_2015,sinasc_2016,sinasc_2017,sinasc_2018,sinasc_2019_2022) %>%
  dplyr::select(cnes_estabelecimento,ibge_estabelecimento,semanas_de_gestacao,tipo_parto,consulta_prenatal...18,
                data_nascimento,parto_cesarea, semana_gestacao,cesarea_anterior_parto,mes_gestacao_prenatal,
                sexo, idade, raça_cor_rn) %>%
  #Retirando linhas com NA:
  tidyr::drop_na(ibge_estabelecimento) %>%
  #Mantendo apenas cidades do estado do PR (iniciando com 41):
  dplyr::filter(substr(ibge_estabelecimento,1,2) == "41") %>%
  dplyr::mutate(data_categorica = ifelse(data_nascimento > "2020-03-20",'Depois','Antes')) %>%
  dplyr::left_join(munic,by = "ibge_estabelecimento") %>%
  dplyr::mutate_at(c("semanas_de_gestacao","tipo_parto","consulta_prenatal...18","parto_cesarea",
                     "cesarea_anterior_parto","macro","municipio","micro","sexo"),as.factor) %>%
  dplyr::rename(consulta_prenatal="consulta_prenatal...18", raca_cor = raça_cor_rn) %>%
  dplyr::mutate(consulta_prenatal = forcats::fct_recode(consulta_prenatal,
                                                        "Ignorado" = "Não informado"),
                semanas_de_gestacao = forcats::fct_recode(semanas_de_gestacao,
                                                          "Ignorado" = "Não informado"),
                cesarea_anterior_parto = forcats::fct_recode(cesarea_anterior_parto,
                                                             "Ignorado" = "Não informado",
                                                             "Ignorado" = "Não se aplica",
                                                             "Ignorado" = "ignorado"),
                parto_cesarea1 = forcats::fct_recode(parto_cesarea,
                                                     "0"="Nenhum",
                                                     "4000"= "Não informado"),
                parto_cesarea1 = as.numeric(as.character(parto_cesarea1)),
                parto_cesarea1 = ifelse(parto_cesarea1 > 36,"Ignorado",
                                        ifelse(parto_cesarea1==0,"Nenhum",
                                               ifelse(parto_cesarea1==1,"Um",
                                                      ifelse(parto_cesarea1== 2,"Dois",
                                                             ifelse(parto_cesarea1 > 2,"Mais que dois",NA))))),
                mes_gestacao_prenatal1 = as.numeric(mes_gestacao_prenatal),
                mes_gestacao_prenatal1 = ifelse(is.na(mes_gestacao_prenatal1) ,"Ignorado",
                                                ifelse(mes_gestacao_prenatal1 >= 1 & mes_gestacao_prenatal1 < 4 ,"1º Trimestre",
                                                       ifelse(mes_gestacao_prenatal1 >= 4 & mes_gestacao_prenatal1 < 7 ,"2º Trimestre",
                                                              ifelse(mes_gestacao_prenatal1 >= 7 & mes_gestacao_prenatal1 < 11 ,"3º Trimestre","Ignorado")))),
                idade = ifelse(idade>=10 & idade<19, "Jovens: 10 a 18 anos",
                               ifelse(idade>=19 & idade < 31, "Adultos Jovens: 19 a 30 anos",
                                      ifelse(idade>=31 & idade<=60,"Adultos: 31 a 59 anos",
                                             ifelse(idade > 60 & idade<90, "Idosos: acima de 60", NA)))),
                raca_cor = forcats::fct_recode(raca_cor,
                                                  "Branca" = "Branca",
                                                  "Não branca" = "Preta",
                                                  "Não branca" = "Amarela",
                                                  "Não branca" = "Indígena",
                                                  "Não branca" = "Parda" )) %>%
  tidyr::drop_na(raca_cor) %>%
  dplyr::mutate_at(c("parto_cesarea1","mes_gestacao_prenatal1"),as.factor) %>%
  dplyr::relocate(municipio,micro,macro,municipio_semacento) %>%
  dplyr::rename(data_variable = data_nascimento) %>%
  dplyr::select(municipio,micro,macro,populacao,data_variable,data_categorica,
                sexo,idade,raca_cor)


usethis::use_data(dados_sinasc_intervencao, overwrite = TRUE)

