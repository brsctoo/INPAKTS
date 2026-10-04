# Helpers dos módulos "descritivo" (SINASC, SIM neonatal, SIM materna, SIF gestante, SIF congênita)
#
# Cada módulo só precisa:
#   1. `escolha <- escolha_descritivo(input, opcoes_usuario)`  (o que o usuário escolheu ao clicar)
#   2. `graficos_descritivo(lista_graficos_desc(grafico_desc(...), grafico_desc(...)), ...)`  (os gráficos)



# 1. Descrever os gráficos

#' Cria uma tabela para descrever um gráfico
#'
#' @return data.frame de uma linha
#' @noRd
grafico_desc <- function(
  id,
  variavel,
  legenda,
  topo = FALSE,
  cache = TRUE,
  titulo = NULL
) {
  data.frame(
    id = id,
    variavel = variavel,
    legenda = legenda,
    posicao_legenda = if (topo) "top" else "none",
    cache = cache,
    titulo = if (is.null(titulo)) NA_character_ else titulo,
    stringsAsFactors = FALSE
  )
}

# Junta os gráficos (que estão em forma de tabela), em uma tabela (na ordem que serão plotados)
lista_graficos_desc <- function(...) {
  do.call(rbind, list(...))
}

# 2. O que o usuário escolheu

#' Salva o que o usuário escolheu, ao clicar no botão, em uma lista
#'
#' @return reactive com `list(nivel, local, data)`
#' @noRd
escolha_descritivo <- function(input, opcoes_usuario, botao = "gerar_graficos") {
  eventReactive(input[[botao]], {
    req(opcoes_usuario$date_intervention[1])
    list(
      nivel = opcoes_usuario$nivel_geografico,
      local = opcoes_usuario$escolha_usuario,
      data  = input$data_selecionada
    )
  })
}


# 3. Os gráficos do módulo

#' Cria os gráficos de um módulo descritivo, um de cada vez (fila) e com cache
#'
#' @param graficos tabela feita com `lista_graficos_desc(grafico_desc(...), ...)`
#' @param indicador "SINASC", "SIM_Neonatal", "SIM_Materno", "SIF_Gestante" ou "SIF_Congenita"
#' @param titulo título dos gráficos (um `titulo` dentro de `grafico_desc()` tem prioridade)
#' @param escolha reactive de `escolha_descritivo()`
#' @param input,output,session os mesmos do módulo
#' @param botao id do botão que dispara os gráficos (sem o prefixo do módulo)
#'
#' @return nada: cria os `output[[id]]` e os observadores
#' @noRd
graficos_descritivo <- function(
  graficos,
  indicador,
  titulo,
  escolha,
  input,
  output,
  session,
  botao = "gerar_graficos"
) {
  # Ids e quantidade de gráficos
  ids <- graficos$id
  n <- length(ids)
  datas_sinasc <- dados_sinasc_intervencao$data_variable


  liberado <- reactiveValues()
  for (g in ids) liberado[[g]] <- 0
  rodada_atual <- reactiveVal(0) # número do último clique

  # Libera o gráfico k+1 logo depois que o k foi enviado para a tela
  avancar <- function(k, rodada) {
    if (rodada != isolate(rodada_atual())) return(invisible()) # houve outro clique: fila velha
    if (k < n) {
      liberado[[ids[k + 1]]] <- rodada # libera para carregar
      session$onFlushed(function() avancar(k + 1, rodada), once = TRUE) # avança ao carregar
    }
  }

  observeEvent(input[[botao]], {
    rodada <- rodada_atual() + 1
    rodada_atual(rodada)
    for (g in ids) liberado[[g]] <- -rodada # os gráficos voltam a "esperando"
    liberado[[ids[1]]] <- rodada # libera o primeiro
    session$onFlushed(function() avancar(1, rodada), once = TRUE) # avança ao carregar
  })

  # Se o conjunto escolhido tiver menos de 30 casos, avisa que os gráficos não serão gerados
  observeEvent(input[[botao]], {
    e <- escolha()
    total <- sum(linhas_desc(indicador, graficos$variavel[1], e$nivel, e$local)$n)
    if (total < 30) {
      shinyalert::shinyalert(
        title = "Banco de dados selecionado possui menos de 30 observações. Gráficos não serão gerados.",
        text = "Por favor, escolha novas opções.", type = "info",
        size = "m")
    }
  })

  # Um renderPlot por linha da tabela `graficos`.
  # (lapply, e não for: cada volta precisa do seu próprio `id`, `variavel`, ...)
  lapply(seq_len(n), function(i) {
    id <- graficos$id[i]
    variavel <- graficos$variavel[i]
    legenda <- graficos$legenda[i]
    posicao <- graficos$posicao_legenda[i]
    titulo_i <- if (is.na(graficos$titulo[i])) titulo else graficos$titulo[i]

    saida <- renderPlot({
      if (liberado[[id]] <= 0) return(invisible(NULL))   # esperando a vez: quadro em branco
      e <- escolha()

      contagem <- contagem_desc(
        indicador, variavel,
        nivel = e$nivel,
        local = e$local,
        data_corte = e$data,
        data_inicio = datas_sinasc
      )

      plot.col1_cubo(contagem, legenda = legenda, titulo = titulo_i, posicao_legenda = posicao)
    })

    if (isTRUE(graficos$cache[i])) {
      saida <- bindCache(
        saida,
        indicador,
        id,
        variavel,
        escolha()$nivel, escolha()$local, escolha()$data,
        liberado[[id]] > 0
      )
    }

    # Desenha quando a "senha" desse gráfico mudar (não ao abrir o app)
    output[[id]] <- bindEvent(saida, liberado[[id]], ignoreInit = TRUE)
  })

  invisible(NULL)
}
