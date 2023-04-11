#' intervencao_sif_congenita UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_intervencao_sif_congenita_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' intervencao_sif_congenita Server Functions
#'
#' @noRd 
mod_intervencao_sif_congenita_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_intervencao_sif_congenita_ui("intervencao_sif_congenita_1")
    
## To be copied in the server
# mod_intervencao_sif_congenita_server("intervencao_sif_congenita_1")
