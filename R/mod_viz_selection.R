#' viz_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz_selection_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' viz_selection Server Functions
#'
#' @noRd 
mod_viz_selection_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
   
  })
}

## To be copied in the UI
# mod_viz_selection_ui("viz_selection_ui_1")

## To be copied in the server
# mod_viz_selection_server("viz_selection_ui_1")
