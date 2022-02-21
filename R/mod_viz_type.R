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
    
    viz_type <- reactive({
      req(r$d_viz)
      df <- r$d_viz
      tv <- "CatNum"
      if (ncol(df) == 3) tv <- "CatCatNum"
      if ("Año_hecho" %in% names(df)) {
        tv <- "YeaNum"
        if (ncol(df) == 3) tv <- "YeaCatNum"
      }
      tv
    })
    
    viz_name <- reactive({
      req(viz_type())
      if (r$active_viz == "table") return()
      paste0("hgchmagic::", paste0("hgch_", r$active_viz, "_", viz_type()))
    })
    
    observe({
      r$v_type <- viz_name()
    })
    
    
  })
}
    
## To be copied in the UI
# mod_viz_type_ui("viz_type_ui_1")
    
## To be copied in the server
# mod_viz_type_server("viz_type_ui_1")
