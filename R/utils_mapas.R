# Helpers do módulo "análise geográfica" (os 5 mapas)
#
# O módulo precisa:
#   1. `escolha <- escolha_mapa(input)`  (o que o usuário escolheu ao clicar)
#   2. `mapas_geograficos(lista_mapas_geo(mapa_geo(...), mapa_geo(...)), escolha, input, output, session)`



# 1. Descrever os mapas
#' Cria uma tabela para descrever um mapa
#'
#' @return lista com os 4 itens
#' @noRd
mapa_geo <- function(id, titulo, base_mun, base_rs) {
  list(
    id = id,
    titulo = titulo,
    base_mun = base_mun,
    base_rs = base_rs
  )
}

# Junta os mapas (que estão em forma de lista), em uma tabela (na ordem de plotagem)
lista_mapas_geo <- function(...) {
  list(...)
}


# 2. O que o usuário escolheu

#' Salva o que o usuário escolheu, ao clicar no botão, em uma lista
#'
#' @return reactive com `list(nivel, local, data)`; `nivel` é "PR" ou "RS"
#' @noRd
escolha_mapa <- function(input, botao = "gerar_graficos") {
  eventReactive(input[[botao]], {
    req(input$data_selecionada)
    list(
      nivel = input$radio,
      local = if (input$radio == "PR") "PR" else input$escolha_usuario,
      data  = as.character(input$data_selecionada)
    )
  })
}


# 3. Montar um mapa

# Estado: mapa por município ao lado do mapa por Regional.
# Regional: mapa da Regional escolhida.
montar_mapa_geo <- function(mapa, escolha) {
  coluna <- escolha$data

  if (escolha$nivel == "RS") {
    RSmapOrd(
      varToPlot = mapa$base_mun[, coluna],
      legeName = "Mudança na tendência",
      mun = mapa$base_mun$municipio,
      RS = escolha$local,
      legeLabels = levels(mapa$base_mun[, coluna]),
      plot.action = FALSE
    )$map
  } else {
    ggpubr::ggarrange(
      UFmapOrd(
        varToPlot = mapa$base_mun[, coluna],
        mun = mapa$base_mun$municipio,
        legeName = "Mudança na tendência por munípio no PR",
        legeLabels = levels(mapa$base_mun[, coluna])
      )$map,
      UFmapOrd(
        varToPlot = mapa$base_rs[, coluna],
        mun = mapa$base_rs$municipio,
        legeName = "Mudança na tendência por RS no PR",
        legeLabels = levels(mapa$base_rs[, coluna])
      )$map
    )
  }
}


# 4. Os mapas do módulo

#' Cria os mapas do módulo, um de cada vez (fila) e com cache
#'
#' @param mapas lista feita com `lista_mapas_geo(mapa_geo(...), ...)`
#' @param escolha reactive de `escolha_mapa()`
#' @param input,output,session os mesmos do módulo
#' @param botao id do botão que dispara os mapas (sem o prefixo do módulo)
#'
#' @return nada: cria os `output$mapa_<id>`, os `output$titulo_<id>` e os observadores
#' @noRd
mapas_geograficos <- function(
  mapas,
  escolha,
  input,
  output,
  session,
  botao = "gerar_graficos"
) {
  ids <- vapply(mapas, function(m) m$id, character(1))
  n <- length(ids)

  liberado <- reactiveValues()
  for (g in ids) liberado[[g]] <- 0
  rodada_atual <- reactiveVal(0) # número do último clique

  # Libera o mapa k+1 logo depois que o k foi enviado para a tela
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
    for (g in ids) liberado[[g]] <- -rodada # os mapas voltam a "esperando"
    liberado[[ids[1]]] <- rodada # libera o primeiro
    session$onFlushed(function() avancar(1, rodada), once = TRUE) # avança ao carregar
  })

  # Um título e um renderPlot por mapa
  # (lapply, e não for: cada volta precisa do seu próprio `m`, `id`, ...)
  lapply(mapas, function(m) {
    id <- m$id

    output[[paste0("titulo_", id)]] <- renderText({
      e <- escolha()
      paste(m$titulo, if (e$nivel == "PR") "do Estado do Paraná" else paste0("da ", e$local, "ª Regional de Saúde"))
    })

    saida <- renderPlot({
      if (liberado[[id]] <= 0) return(invisible(NULL)) # esperando a vez: quadro em branco
      montar_mapa_geo(m, escolha())
    })

    saida <- bindCache(
      saida,
      id,
      escolha()$nivel, escolha()$local, escolha()$data,
      liberado[[id]] > 0
    )

    # Desenha quando a "senha" desse mapa mudar (não ao abrir o app)
    output[[paste0("mapa_", id)]] <- bindEvent(saida, liberado[[id]], ignoreInit = TRUE)
  })

  invisible(NULL)
}
