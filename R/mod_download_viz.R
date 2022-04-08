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
    uiOutput(ns("descargas"))
  )
}

#' download_viz Server Functions
#'
#' @noRd 
mod_download_viz_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$descargas <- renderUI({
      
      if (is.null(r$active_viz)) return()
      if (r$active_viz != "table") {
        dsmodules::downloadImageUI(ns("download_viz"), dropdownLabel ="Descargar", text = "Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
      } else {
        dsmodules::downloadTableUI(ns("dropdown_table"), dropdownLabel = "Descargar", text = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
      }
    })
    
    observe({
      dsmodules::downloadTableServer("dropdown_table", element = reactive(r$d_fil), formats = c("csv", "xlsx", "json"))
      dsmodules::downloadImageServer("download_viz", element = reactive(r$downViz), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
    })
    
  })
}

## To be copied in the UI
# mod_download_viz_ui("download_viz_ui_1")

## To be copied in the server
# mod_download_viz_server("download_viz_ui_1")
