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
      req(r$active_viz)
      df <- r$d_viz
      tv <- "CatNum"
      if (ncol(df) == 3) tv <- "CatCatNum"
      if ("Año_hecho" %in% names(df)) {
        tv <- "YeaNum"
        if (ncol(df) == 3) tv <- "YeaCatNum"
      }
      if ("latitud" %in% names(df)) {
        if (ncol(df) == 3) tv <- "GlnGltNum"
        if (ncol(df) == 4) tv <- "GlnGltCatNum"
      }
      if (r$active_viz == "choropleth") {
       if (ncol(df) == 2) tv <- "GnmNum"
       if (ncol(df) == 3) tv <- "GnmCatNum"
      }
      print(tv)
      tv
    })
    
    viz_name <- reactive({
      req(viz_type())
      if (r$active_viz == "table") return()
      if (r$active_viz %in% c("choropleth", "bubbles")) {
      vp <- paste0("lfltmagic::", paste0("lflt_", r$active_viz, "_", viz_type())) 
      } else {
      vp <- paste0("hgchmagic::", paste0("hgch_", r$active_viz, "_", viz_type()))
      }
      print(vp)
      vp
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
