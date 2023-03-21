#' sobre UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sobre_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(sidebarLayout(
    position = "right",
    sidebarPanel(
      style = "background-color: #ffffff;",
      align = "center",
      width = 5,
      #Seção de parceiros
      bs4Dash::infoBox(
        width = 12,
        icon = icon(""),
        tags$h2("Parceria"),
        tags$a(img(
          src = "www/Slide2.png",
          width = 250,
          height = 60
        ),
        href = "https://www.saude.pr.gov.br/")
      ),
      br(),
      # Seção Financiamento
      bs4Dash::infoBox(
        width = 12,
        icon = icon(""),
        tags$h2("Financiamento"),
        tags$ul(
          tags$img(
            src = "www/Slide3.png",
            width = 450,
            height = 80
          ),
          br(),
          br(),
          tags$img(
            src = "www/Slide4.png",
            width = 200,
            height = 80
          ),
          br(),
          br(),
          tags$img(
            src = "www/Slide5.png",
            width = 150,
            height = 80
          ),
          br(),
          br(),
          tags$img(
            src = "www/Slide6.png",
            width = 150,
            height = 80
          ),
          br(),
          br(),
          tags$img(
            src = "www/Slide7.png",
            width = 150,
            height = 80
          )
        )
      )
    ),
    mainPanel(
      width = 7,
      #logo superior
      tags$a(
        img(
          src = "www/Slide1.png",
          width = "100%",
          height = 125,
          align = "center"
        ),
        href = "http://www.des.uem.br/"
      ),
      br(),
      br(),
      # Seção Equipe de Desenvolvimento
      bs4Dash::infoBox(
        icon = icon("people-group"),
        width = 12,
        tags$h2("Equipe de Desenvolvimento"),
        tags$p("Conheça nossa equipe de desenvolvimento:"),
        tags$ul(
          tags$li("Nome do Bolsista 1"),
          tags$li("Nome do Bolsista 2"),
          tags$li("Nome do Bolsista 3"),
          tags$li("Coordenadora: Eniuce Menezes")
        )
      ),
      # Seção Equipe de Colaboradores
      bs4Dash::infoBox(
        icon = icon("people-group"),
        width = 12,
        tags$h2("Equipe de Colaboradores"),
        tags$p("Conheça nossos colaboradores: "),
        tags$ul(
          tags$li(
            "SESA-PR: Acácia Maria Lourenço Francisco Nasr; Maria Goretti David Lopes"
          ),
          tags$li("DES/UEM: Adrian Strieder Philippsen; Diogo Rossoni"),
          tags$li("DEM/UEM: Fernanda Nishida, Luciano Andrade"),
          tags$li("DEF/UEM Raissa Bocchi Pedroso, Rosana Rosseto Oliveira"),
          tags$li("UNINGÁ:  Sandra Marisa Pelloso"),
          tags$li("15ª Regional de Saúde-PR: Greicy Cesar do Amaral"),
          tags$li("Duke University: João Ricardo Nickenig Vissoci")
        )
      )
    )
  )))
}

#' sobre Server Functions
#'
#' @noRd
mod_sobre_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sobre_ui("sobre_ui_1")

## To be copied in the server
# mod_sobre_server("sobre_ui_1")
