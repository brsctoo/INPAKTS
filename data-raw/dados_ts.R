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

indicadores <- list(
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

bases <- lapply(indicadores, function(x) preparar(x$dados))

dados_ts_painel <- lapply(names(indicadores), function(ind) {
  bases[[ind]] %>%
    contar() %>%
    mutate(indicador = ind)
}) %>%
  bind_rows() %>%
  select(indicador, nivel, local, DATA, total_casos) %>%
  arrange(indicador, nivel, local, DATA)

dados_ts_categorias <- lapply(names(indicadores), function(ind) {
  dims <- indicadores[[ind]]$dims
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

# Conferências ------------------------------------------------------------------

# 1. chaves únicas
stopifnot(
  !anyDuplicated(dados_ts_painel[c("indicador", "nivel", "local", "DATA")]),
  !anyDuplicated(dados_ts_categorias[c("indicador", "nivel", "local", "DATA",
                                       "dimensao", "categoria")])
)

# 2. totais por indicador e nível batem com o dataset
resumo <- lapply(names(indicadores), function(ind) {
  df <- bases[[ind]]
  g  <- dados_ts_painel[dados_ts_painel$indicador == ind, ]

  stopifnot(sum(g$total_casos[g$nivel == "PR"]) == nrow(df))
  for (n in c("macro", "micro", "municipio")) {
    stopifnot(sum(g$total_casos[g$nivel == n]) == sum(!is.na(df[[n]])))
  }

  data.frame(indicador      = ind,
             casos          = nrow(df),
             sem_localidade = sum(is.na(df$municipio)),
             primeiro_mes   = min(df$DATA),
             ultimo_mes     = max(df$DATA),
             linhas_cubo    = nrow(g))
}) %>% bind_rows()

# 3. em cada dimensão, a soma das categorias é igual ao cubo geral
soma_cat <- dados_ts_categorias %>%
  group_by(indicador, dimensao, nivel, local, DATA) %>%
  summarise(total = sum(total_casos), .groups = "drop") %>%
  left_join(dados_ts_painel, by = c("indicador", "nivel", "local", "DATA"))

n_dims <- vapply(indicadores, function(x) length(x$dims), integer(1))

stopifnot(
  !anyNA(soma_cat$total_casos),
  all(soma_cat$total == soma_cat$total_casos),
  nrow(soma_cat) == sum(n_dims[dados_ts_painel$indicador])
)

cat("Cubo gerado em", format(round(difftime(Sys.time(), inicio, units = "secs"), 1)), "\n")
print(resumo)
cat("Linhas: painel =", nrow(dados_ts_painel),
    "| categorias =", nrow(dados_ts_categorias), "\n")

# Salva em data/ ------------------------------------------------------------------

usethis::use_data(dados_ts_painel, dados_ts_categorias, overwrite = TRUE)
