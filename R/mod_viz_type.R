#' viz_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz_type_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' viz_type Server Functions
#'
#' @noRd 
mod_viz_type_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    
  })
}
    
## To be copied in the UI
# mod_viz_type_ui("viz_type_ui_1")
    
## To be copied in the server
# mod_viz_type_server("viz_type_ui_1")
