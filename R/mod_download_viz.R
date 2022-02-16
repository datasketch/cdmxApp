#' download_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
#' download_viz Server Functions
#'
#' @noRd 
mod_download_viz_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
   
    
  })
}
    
## To be copied in the UI
# mod_download_viz_ui("download_viz_ui_1")
    
## To be copied in the server
# mod_download_viz_server("download_viz_ui_1")
