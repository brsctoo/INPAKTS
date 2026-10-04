# Cubo de dados (contagens mensais pré-calculadas) -----------------------------
#
# Gera duas tabelas em data/:
#
#   dados_ts_painel      -> série geral
#     indicador | nivel | local | DATA | total_casos
#
#   dados_ts_categorias  -> séries dos subgráficos (uma dimensão por vez)
#     indicador | nivel | local | DATA | dimensao | categoria | total_casos
#
# Regras:
#   * DATA é o primeiro dia do mês (Date). O mês é calculado com a mesma
#     expressão usada no app: as.Date(floor_date(data_variable, "month")).
#   * Só existem linhas para os meses COM casos. Os zeros são preenchidos na
#     leitura, sobre o calendário (janela) do indicador.
#   * nivel: "PR", "macro", "micro" ou "municipio".
#       - PR conta todas as linhas do dataset (inclusive sem localidade).
#       - Os demais níveis ignoram linhas com localidade vazia (NA).
#   * categoria guarda todos os valores da coluna, inclusive "N.I."/"Não
#     informado"; valores vazios (NA) viram o texto "NA".
#
# Como rodar: depois de atualizar os datasets dados_*_intervencao /
# dados_*_intervention, com o pacote carregado (devtools::load_all()),
# execute este script inteiro. Se alguma conferência falhar, ele para.

library(dplyr)

# Indicadores e dimensões -------------------------------------------------------
# dims: nome da dimensão no cubo = coluna do dataset

indicadores_intervencao <- list(
  SINASC = list(
    dados = dados_sinasc_intervencao,
    dims  = c(sexo = "sexo", idade = "idade", raca = "raca_cor")),
  SIM_Materno = list(
    dados = dados_sim_materno_intervention,
    dims  = c(idade = "idade", raca = "raca_cor")),
  SIM_Neonatal = list(
    dados = dados_sim_neonatal_intervention,
    dims  = c(sexo = "sexo", tipo = "tipo_mortalidade", raca = "raca_cor")),
  SIF_Gestante = list(
    dados = dados_sif_gestante_intervention,
    dims  = c(idade = "idade", raca = "raca_cor")),
  SIF_Congenita = list(
    dados = dados_sif_congenita_intervention,
    dims  = c(idade = "idade", raca = "raca_cor"))
)

# Funções auxiliares ------------------------------------------------------------

# Cria a coluna DATA (1º dia do mês) e descarta linhas sem data
preparar <- function(df) {
  df %>%
    mutate(DATA = as.Date(lubridate::floor_date(data_variable, "month"))) %>%
    filter(!is.na(DATA))
}

# Conta casos por mês nos 4 níveis; `extra` = colunas adicionais de agrupamento
contar <- function(df, extra = character()) {
  chaves <- c("DATA", extra)

  pr <- df %>%
    count(across(all_of(chaves)), name = "total_casos") %>%
    mutate(nivel = "PR", local = "PR")

  sub <- lapply(c("macro", "micro", "municipio"), function(n) {
    df %>%
      filter(!is.na(.data[[n]])) %>%
      count(local = as.character(.data[[n]]),
            across(all_of(chaves)), name = "total_casos") %>%
      mutate(nivel = n)
  })

  bind_rows(pr, sub)
}

# Construção --------------------------------------------------------------------

inicio <- Sys.time()

bases <- lapply(indicadores_intervencao, function(x) preparar(x$dados))

dados_ts_painel <- lapply(names(indicadores_intervencao), function(ind) {
  bases[[ind]] %>%
    contar() %>%
    mutate(indicador = ind)
}) %>%
  bind_rows() %>%
  select(indicador, nivel, local, DATA, total_casos) %>%
  arrange(indicador, nivel, local, DATA)

dados_ts_categorias <- lapply(names(indicadores_intervencao), function(ind) {
  dims <- indicadores_intervencao[[ind]]$dims
  lapply(names(dims), function(dim) {
    bases[[ind]] %>%
      mutate(categoria = coalesce(as.character(.data[[dims[[dim]]]]), "NA")) %>%
      contar("categoria") %>%
      mutate(indicador = ind, dimensao = dim)
  }) %>% bind_rows()
}) %>%
  bind_rows() %>%
  select(indicador, nivel, local, DATA, dimensao, categoria, total_casos) %>%
  arrange(indicador, dimensao, nivel, local, categoria, DATA)

# Salva em data/ ------------------------------------------------------------------

usethis::use_data(dados_ts_painel, dados_ts_categorias, overwrite = TRUE)

# ================================

indicadores_descritivo <- list(
  SINASC = list(
    dados = dados_sinasc,
    dims  = c(
      consulta_prenatal = "consulta_prenatal",
      parto_cesarea1 = "parto_cesarea1",
      mes_gestacao_prenatal1 = "mes_gestacao_prenatal1",
      tipo_parto = "tipo_parto",
      cesarea_anterior_parto = "cesarea_anterior_parto",
      semanas_de_gestacao = "semanas_de_gestacao"
    )),
  SIM_Neonatal = list(
    dados = dados_sim_neonatal,
    dims  = c(
      tipo_gestacao = "tipo_gestacao",
      tipo_parto = "tipo_parto",
      tipo_morte_parto = "tipo_morte_parto",
      tipo_obito = "tipo_obito",
      local_ocorrencia = "local_ocorrencia",
      raca_cor = "raca_cor",
      escolaridade_mae = "escolaridade_mae"
    )),
  SIM_Materno = list(
    dados = dados_sim_materno,
    dims  = c(
      morte_puerperio = "morte_puerperio",
      local_ocorrencia = "local_ocorrencia",
      morte_mulher = "morte_mulher",
      tp_morte_ocorreu = "tp_morte_ocorreu",
      escolaridade = "escolaridade",
      raca_cor = "raca_cor",
      estado_civil = "estado_civil"
    )),
  SIF_Gestante = list(
    dados = dados_sif_gestante,
    dims = c(
      CS_RACA = "CS_RACA",
      idade_mae1 = "idade_mae1",
      TPEVIDENCI = "TPEVIDENCI",
      CS_ESCOL_N = "CS_ESCOL_N",
      TPTESTE1 = "TPTESTE1",
      TPCONFIRMA = "TPCONFIRMA"
    )),
  SIF_Congenita = list(
    dados = dados_sif_congenita,
    dims = c(
      idade1 = "idade1",
      CS_RACA = "CS_RACA",
      EVO_DIAG_N = "EVO_DIAG_N",
      ANTSIFIL_N = "ANTSIFIL_N"
    ))
)

# 01. Coloca uma nova coluna de DATAS arredondas
dados_dt_com_data <- list()

for (ind in names(indicadores_descritivo)) {
  tabela <- indicadores_descritivo[[ind]]$dados
  tabela <- preparar(tabela)
  dados_dt_com_data[[ind]] <- tabela
}

# 02. Conta a quantidade de casos e junta todas as tabelas
tabelas <- list()

for (ind in names(indicadores_descritivo)) {
  dims <- indicadores_descritivo[[ind]]$dims
  tabela <- dados_dt_com_data[[ind]]          # singular

  for (variavel in names(dims)) {
    coluna <- dims[[variavel]]
    contagem <- tabela %>%
      mutate(categoria = coalesce(as.character(.data[[coluna]]), "NA")) %>%
      contar("categoria") %>%
      mutate(indicador = ind, variavel = variavel)
    tabelas[[paste(ind, variavel)]] <- contagem
  }
}

dados_desc_painel <- tabelas %>%
bind_rows() %>%
select(indicador, variavel, nivel, local, DATA, categoria, n = total_casos) %>%
arrange(indicador, variavel, nivel, local, DATA, categoria)

# 03. Padroniza o nome dos municípios
# Os datasets do SIM trazem o município sem acento e em minúsculas ("abatia"), e a tela
# (`municipios_PR`, na barra lateral) oferece o nome oficial ("Abatiá"). Aqui, no nível
# "municipio", o `local` passa a ser sempre o nome oficial.

oficiais <- as.character(municipios_PR$municipio)

# Chave: minúscula e só letras e números
chave_nome <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# chave -> nome oficial
chave_para_oficial <- stats::setNames(
  as.character(dengueControl::munic$MUNICIPIO),
  chave_nome(dengueControl::munic$SEM_ACENTO))
stopifnot(all(chave_para_oficial %in% oficiais), !anyDuplicated(names(chave_para_oficial)))

# Nomes do SIM escritos de outro jeito (como o SIM escreve -> como dengueControl escreve)
correcoes <- c(
  "munhoz de melo" = "munhoz de mello",
  "santa cruz de monte castelo" = "santa cruz monte castelo")
correcoes <- stats::setNames(chave_nome(correcoes), chave_nome(names(correcoes)))

padronizar_municipio <- function(local) {
  chave <- chave_nome(local)
  chave <- ifelse(chave %in% names(correcoes), correcoes[chave], chave)
  novo <- unname(chave_para_oficial[chave])
  ifelse(local %in% oficiais, local, novo)
}

antes <- dados_desc_painel

dados_desc_painel <- dados_desc_painel %>%
  mutate(local = if_else(nivel == "municipio", padronizar_municipio(local), local)) %>%
  filter(nivel != "municipio" | !is.na(local)) %>%
  group_by(indicador, variavel, nivel, local, DATA, categoria) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(indicador, variavel, nivel, local, DATA, categoria)

lista_niveis <- list()

for (ind in names(indicadores_descritivo)) {
  dims   <- indicadores_descritivo[[ind]]$dims
  tabela <- dados_dt_com_data[[ind]]

  for (variavel in names(dims)) {
    coluna <- dims[[variavel]]
    x      <- tabela[[coluna]]

    ordem <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
    if (anyNA(x)) ordem <- c(ordem, "NA")

    lista_niveis[[paste(ind, variavel)]] <- data.frame(
      indicador = ind,
      variavel  = variavel,
      categoria = ordem,
      ordem     = seq_along(ordem))
  }
}

dados_desc_niveis <- bind_rows(lista_niveis)

# Salva em data/

usethis::use_data(dados_desc_painel, dados_desc_niveis, overwrite = TRUE)
