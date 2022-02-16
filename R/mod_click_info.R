#' click_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_click_info_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}
    
#' click_info Server Functions
#'
#' @noRd 
mod_click_info_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
        
  })
}
    
## To be copied in the UI
# mod_click_info_ui("click_info_ushi18ny::i_1")
    
## To be copied in the server
# mod_click_info_server("click_info_ushi18ny::i_1")
