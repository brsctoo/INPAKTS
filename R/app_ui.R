#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # needed for shinyjs
    shinyjs::useShinyjs(), # Include shinyjs
    rintrojs::introjsUI(), # Required to enable introjs scripts
    # Your application UI logic
    navbarPage(
      id = "intabset", # needed for landing page
      title = div(tags$a(img(src = "www/Logo2.png", height = 70, width = 130)
      ),
      style = "position: relative; top: 4px;"
      ), # Navigation bar

      windowTitle = "SESA", # title for browser tab
      # Usando bottstrap versão 4
      theme = bslib::bs_theme(version = 4),
      #theme = shinythemes::shinytheme(theme = "cosmo"), # opões para Theme: cerulean e cosmo
      collapsible = FALSE, # tab panels collapse into menu in small screens
      header = tags$head(),

      # Apresentação ----
      tabPanel(
        title = "Apresentação",
        icon = icon("home"),
        value = "apresentacao",
        mod_apresentacao_ui("apresentacao_ui_1")
      ),



      # Análise de intervenção ----
      navbarMenu(
        title = "Análise de intervenção",
        icon = icon("globe"),
        #id = "analise",

        ## SINASC ----
        tabPanel(
          title = "SINASC",
          value = "interv_sinasc",
          mod_intervencao_sinasc_ui("intervencao_sinasc_1")
        ),

        ## SIM NEONATAL ----
        tabPanel(
          title = "SIM NEONATAL",
          value = "interv_sim_neonatal",
          mod_intervencao_sim_neonatal_ui("intervencao_sim_neonatal_1")
        ),

        ## SIM MATERNO ----
        tabPanel(
          title = "SIM MATERNO",
          value = "interv_sim_materno",
          mod_intervencao_sim_materno_ui("intervencao_sim_materno_1")
        ),

        ## SIF GESTANTE ----
        tabPanel(
          title = "SIFÍLIS GESTANTE",
          value = "interv_sif_gestante",
          mod_intervencao_sif_gestante_ui("intervencao_sif_gestante_1")
        ),
      ),


      # Análise descritiva -------
      navbarMenu(
        title = "Análise descritiva",
        icon = icon("globe"),

        ## SINASC ----
        tabPanel(
          title = "SINASC",
          value = "desc_sinasc",
          mod_descritiva_sinasc_ui("descritiva_sinasc_1")
        ),

        ## SIM - Neonatal ----
        tabPanel(
          title = "SIM NEONATAL",
          value = "desc_sim_neonatal",
          mod_descritiva_sim_neonatal_ui("descritiva_sim_neonatal_1")
        ),

        ## SIM - Mortalidade ----
        tabPanel(
          title = "SIM MATERNA",
          value = "desc_sim_materna",
          mod_descritiva_sim_materna_ui("descritiva_sim_materna_1")
        ),

        ## SIF - Gestante ----
        tabPanel(
          title = "SIFÍLIS GESTANTE",
          value = "desc_sif_gestante",
          mod_descritiva_sif_gestante_ui("descritiva_sif_gestante_1")
        ),

        ## SIF - Congenita ----
        tabPanel(
          title = "SIFÍLIS CONGENITA",
          value = "desc_sif_congenita",
          mod_descritiva_sif_congenita_ui("descritiva_sif_congenita_1")
        ),

      ),


      # Análise geográfica ----
      tabPanel(
        title = "Análise geográfica",
        icon = icon("globe"),
        mod_analise_geografica_ui("analise_geografica_1")
      ),

      # Sobre --------
      navbarMenu(
        title = "Sobre",
        icon = icon("info-circle"),
        tabPanel(
          title = "Sobre",
          value = "sobre",
          mod_sobre_ui("sobre_ui_1")
        )
      )#,

      # navbarMenu(
      #   title = "teste",
      #   icon = icon("info-circle"),
      #  tabPanel(
      #    title = "Sobre",
      #     value = "teste1",
      #    mod_testando_ui("testando_1")
      #  )
      # )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#'
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    #Criando pagina inicial:
    waiter::useWaiter(),

    cookie_box,
    #Favicon trata-se da figura bem pequena que aparece na aba do navegador
    favicon(ico = "fig1",ext = "jpeg"),
    # bundle_resources(), links all the CSS and JavaScript files contained in
    #inst/app/www to your application, so you don’t have to link them manually.
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SESA"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyalert::useShinyalert(force = TRUE)
  )
}

