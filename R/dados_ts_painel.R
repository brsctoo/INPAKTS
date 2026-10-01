#' Cubo de dados: contagens mensais por indicador e local
#'
#' Tabelas com contagens mensais de casos, pré-calculadas por
#' `data-raw/dados_ts.R`, usadas nos gráficos e tabelas da aba de intervenção.
#' Guardam apenas os meses com casos; os zeros são preenchidos na leitura,
#' sobre o calendário do SINASC (ver `R/utils_ts.R`).
#'
#' @format `dados_ts_painel`: data.frame com 5 colunas.
#' \describe{
#'   \item{indicador (character)}{`"SINASC"`, `"SIM_Materno"`, `"SIM_Neonatal"`, `"SIF_Gestante"` ou `"SIF_Congenita"`}
#'   \item{nivel (character)}{`"PR"`, `"macro"`, `"micro"` ou `"municipio"`}
#'   \item{local (character)}{nome do local no nível (`"PR"` no nível PR)}
#'   \item{DATA (Date)}{primeiro dia do mês}
#'   \item{total_casos (integer)}{número de casos no mês}
#' }
#' @source Gerado por `data-raw/dados_ts.R` a partir dos datasets `dados_*_intervencao`.
"dados_ts_painel"

#' @rdname dados_ts_painel
#'
#' @format `dados_ts_categorias`: data.frame com 7 colunas (as 5 acima mais
#' `dimensao` e `categoria`, antes de `total_casos`).
#' \describe{
#'   \item{dimensao (character)}{`"sexo"`, `"idade"`, `"raca"` ou `"tipo"`}
#'   \item{categoria (character)}{valor da categoria; vazio (NA) vira `"NA"`}
#' }
"dados_ts_categorias"

#Após realizar qualquer alteração neste arquivo. Use o comando `devtools::document()`
#para a documentação ser atualizado nos arquivos "man/dados_ts_painel.Rd" e "man/dados_ts_categorias.Rd"
