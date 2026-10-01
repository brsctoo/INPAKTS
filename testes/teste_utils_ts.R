# Teste de equivalência: série do cubo (utils_ts.R) x série antiga (data_prep + return_ts)
# Rodar na raiz do projeto, depois de devtools::load_all()
library(dplyr)

indicadores <- list(
  SINASC        = list(dados = dados_sinasc_intervencao,         dims = c(sexo = "sexo", idade = "idade", raca_cor = "raca")),
  SIM_Materno   = list(dados = dados_sim_materno_intervention,   dims = c(idade = "idade", raca_cor = "raca")),
  SIM_Neonatal  = list(dados = dados_sim_neonatal_intervention,  dims = c(sexo = "sexo", tipo_mortalidade = "tipo", raca_cor = "raca")),
  SIF_Gestante  = list(dados = dados_sif_gestante_intervention,  dims = c(idade = "idade", raca_cor = "raca")),
  SIF_Congenita = list(dados = dados_sif_congenita_intervention, dims = c(idade = "idade", raca_cor = "raca"))
)
locais <- list(c("PR", "PR"), c("macro", "LESTE"), c("municipio", "Curitiba"),
               c("municipio", "Cantagalo"), c("municipio", "Abatiá"))

n <- 0; dif <- 0
for (ind in names(indicadores)) {
  d <- indicadores[[ind]]$dados; dims <- indicadores[[ind]]$dims
  for (lc in locais) {
    x  <- data_prep(d, nivel_geografico = lc[1], local = lc[2])          # forma antiga
    cx <- ts_categorias(ind, nivel = lc[1], local = lc[2])               # forma nova

    # série geral
    a <- return_ts(x, data_variable, inicio = c(2015, 1), tipo = "mensal")
    b <- serie_ts(ind, nivel = lc[1], local = lc[2])
    n <- n + 1; dif <- dif + !isTRUE(all.equal(as.numeric(a), b))

    # série de cada categoria de cada dimensão
    for (v in names(dims)) for (ct in sort(unique(as.character(d[[v]][!is.na(d[[v]])])))) {
      a2 <- x[!is.na(x[[v]]) & x[[v]] == ct, ] %>% return_ts(data_variable, inicio = c(2015, 1), tipo = "mensal")
      b2 <- cx %>% dplyr::filter(dimensao == dims[[v]], categoria == ct) %>% serie_meses()
      n <- n + 1; dif <- dif + !isTRUE(all.equal(as.numeric(a2), b2))
    }
    # total de casos da dimensão (usado nas regras de mínimo de observações)
    for (v in names(dims)) { n <- n + 1; dif <- dif + (nrow(x) != total_casos_cat(cx, dims[[v]])) }
  }
}
cat(sprintf("Comparações: %d | diferenças: %d\n", n, dif))
stopifnot(dif == 0)
