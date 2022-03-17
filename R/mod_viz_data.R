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
     
      req(r$v_sel)
   
      var_sel <- r$v_sel
      
      if (r$active_viz == "map") {
      if (r$mapType  %in% c("bubbles", "heatmap")) {
        var_sel <- c("longitud", "latitud", var_sel)
      }
      }
 
      if (sum("Categoria" %in% var_sel) == 1) {
        if (r$categoriaId != "TODOS") {
          var_sel <- "Delito"
        }
      }
      
      varAnio <- NULL
      if (r$active_viz == "line") {
        req(r$fechasId)
        varAnio  <-  r$fechasId
      }
   
      df <- df[,unique(c(var_sel, varAnio, "AlcaldiaHechos"))] %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(Víctimas = dplyr::n())

      # if (length(unique(df$Año_hecho)) == 1 | r$active_viz != "line") {
      #   indAnio <- grep("Año_hecho", names(df))
      #   df <- df[,-indAnio]
      # }
   
      if (!(r$active_viz %in% c("map"))) {
        if (!("AlcaldiaHechos" %in% var_sel)) {
        df <- df %>% dplyr::select(-AlcaldiaHechos)
        }
      }
      
      if (r$active_viz == "map") {
      if (r$mapType %in% c("choropleth")) {
          df <- df %>% dplyr::select(AlcaldiaHechos, dplyr::everything())
      }}
      
      if (r$active_viz == "map") {
      if (r$mapType %in% c("bubbles","heatmap")) {
        indAlc <- grep("AlcaldiaHechos", names(df))
        df <- df[,-indAlc] %>% tidyr::drop_na()
      }}
 
      if (length(var_sel) == 2 & !(r$active_viz %in% c("line", "map"))) {
        if (is.null(r$axisId)) return()
        #print(r$axisId)
        if (r$axisId) {
          var_sel <- rev(var_sel)
          df <- df[,c(var_sel, "Víctimas")]
        }
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
