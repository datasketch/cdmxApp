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
      req(r$d_fil)
      df <- r$d_fil
      req(r$varViewId)
      
      var_sel <- r$varViewId 
      if (length("Categoria" == var_sel) == 1) {
        if (r$categoriaId != "TODAS") {
          var_sel <- "Delito"
        }
      }
      
      df <- df[,c(var_sel, "Año_hecho")] %>%
               dplyr::group_by_all() %>%
                 dplyr::summarise(Víctimas = dplyr::n())
      if (length(unique(df$Año_hecho)) == 1 | r$active_viz != "line") {
        df <- df %>% dplyr::select(-Año_hecho)
      }
      df
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
