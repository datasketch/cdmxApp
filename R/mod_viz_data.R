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
    
    data_viz <- reactive({
 
      tryCatch({
        req(r$d_fil)
        req(r$aggId)
        varNum <- r$varNum
        varCats <- r$v_sel
        varDate <- NULL
        haveDate <- FALSE
        if (r$active_viz %in% c("line", "area"))  {
          req(r$datesSelected)
          varDate <- r$datesSelected
          if (varCats == "Histórico CDMX") varCats <- NULL
          varCats <- c(varCats, varDate)
          haveDate <- TRUE
        }
        varSel <- c(varCats, varNum)
        df <- r$d_fil
        df <- selectTbl(df, 
                        agg = r$aggId, 
                        varToSel = varSel,
                        varToGroup = varCats, 
                        varToAgg = varNum, 
                        haveDate = haveDate,
                        varDate = varDate)
        #print(df)
        df
      },
      error = function(cond) {
        return()
      })
    })
    
    
    observe({
      r$d_viz <- data_viz()
    })
    
  })
}

## To be copied in the UI
# mod_viz_data_ui("viz_data_ui_1")

## To be copied in the server
# mod_viz_data_server("viz_data_ui_1")
