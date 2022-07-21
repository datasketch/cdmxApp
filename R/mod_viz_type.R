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
      tryCatch({
        req(r$d_viz)
        req(r$active_viz)
        if (r$active_viz %in% c("map_bubbles", "map_heat")) return()
        df <- r$d_viz
        tv <- "CatNum"
        if (ncol(df) == 3) tv <- "CatCatNum"
        if (sum(grepl("Mes|Año|Fecha|fecha", names(df))) > 0) {
          tv <- "YeaNum"
          if (ncol(df) == 3) tv <- "CatYeaNum"
        }
        if (r$active_viz %in% "map") tv <- "GnmNum"
        if (r$active_viz %in% "scatter") {
          req(r$allNums)
          tv <- "CatNumNum"
          if (ncol(df) == 4) tv <- "CatNumNumNum"
          if (is.null(r$allCats)) tv <- gsub("Cat", "", tv)
        }
        tv
      },
      error = function(cond) {
        return()
      })
    })
    
    viz_name <- reactive({
      tryCatch({
        req(viz_type())
        if (r$active_viz == "table") return()
        if (r$active_viz %in% c("map_bubbles", "map_heat")) return()
        if (r$active_viz %in% "map") {
          vp <- paste0("lfltmagic::", paste0("lflt_", "choropleth", "_", viz_type()))
        } else {
          vp <- paste0("hgchmagic::", paste0("hgch_", r$active_viz, "_", viz_type()))
        }
        #print(vp)
        vp
      },
      error = function(cond) {
        return()
      })
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