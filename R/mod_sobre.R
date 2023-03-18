#' sobre UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sobre_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(width=9,
              h4("Sobre", style = "color:black;"), #opcoes de color: black, white
              p("ScotPHO's profiles tool allows users to explore the various different profiles produced by the ", 
                tags$a(href="http://www.scotpho.org.uk/about-us/about-scotpho/", "ScotPHO collaboration.", class="externallink")),
              p("The profiles are intended to increase understanding of local health issues and to prompt further 
                                    investigation, rather than to be used as a performance  management tool. The information needs to be 
                                    interpreted within a local framework; an indicator may be higher or lower in one area compared to another,  but
                                    local knowledge is needed to understand and interpret differences."),
              p("The Scottish Public Health Observatory (ScotPHO) collaboration is led  by Public Health Scotland, and includes
                                    Glasgow Centre for Population Health, National Records of Scotland,  the MRC/CSO Social and Public Health Sciences 
                                      Unit and the Scottish Learning Disabilities Observatory."),
              p("We aim to provide a clear picture of the health of the Scottish population and the factors that affect it. We
                                    contribute to improved collection and use of routine data on health, risk factors, behaviours and wider health 
                                    determinants. We take a lead in determining Scotland's future public health information needs, develop innovations 
                                    in public health  information and provide a focus for new routine public health information development where 
                                      gaps exist."),
              p("If you have any trouble accessing any information on this site or have any further questions or feedback relating
                                      to the data or the tool, then please contact us at: ", tags$b(tags$a(href="mailto:phs.scotpho@phs.scot", "phs.scotpho@phs.scot", class="externallink")),
                "and we will be happy to help."))
 
  )
}
    
#' sobre Server Functions
#'
#' @noRd 
mod_sobre_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sobre_ui("sobre_ui_1")
    
## To be copied in the server
# mod_sobre_server("sobre_ui_1")
