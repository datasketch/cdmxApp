#' viz_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' viz_data Server Functions
#'
#' @noRd 
mod_viz_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    viz <- reactiveValues(data = NULL)
    
    observe({
      req(r$active_viz)
      req(r$d_fil)
      req(r$aggId)
      tryCatch({
      vizSel <- r$active_viz
      varNum <- r$varNum
      if (vizSel %in% c("scatter")) {
        if (is.null(varNum)) varNum <-  r$allNums[1:2]
      }

      varCats <- r$v_sel
      varDate <- NULL
      haveDate <- FALSE
      
      if (vizSel %in% c("line", "area"))  {
        req(r$datesSelected)
        varDate <- r$datesSelected
        if (varCats == "Histórico CDMX") varCats <- NULL
        varCats <- c(varCats, varDate)
        haveDate <- TRUE
      }
      if (vizSel %in% c("map_bubbles")){
        if (is.null(r$coorToPlot)) return()
        varCats <- r$coorToPlot
      }
      varSel <- c(varCats, varNum)
      df <- r$d_fil
      df <- selectTbl(df, 
                      agg = r$aggId, 
                      varToSel = varSel,
                      varToGroup = varCats, 
                      varToAgg = varNum, 
                      haveDate = haveDate,
                      varDate = varDate,
                      viz = vizSel)
      
      if (vizSel == "scatter") {
        if (ncol(df) == 2) return()
      } else {
        if (length(varNum) > 2) return()
      }
      print(df)
      viz$data <- df
      },
      error = function(cond) {
        return()
      })
    })
    
    
    observe({
      r$d_viz <- viz$data
    })
    
  })
}

## To be copied in the UI
# mod_viz_data_ui("viz_data_ui_1")

## To be copied in the server
# mod_viz_data_server("viz_data_ui_1")
