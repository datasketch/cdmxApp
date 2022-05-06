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
      df <- r$d_fil
      req(r$v_sel)
      var_sel <- r$v_sel
      if ("cdmx" %in% var_sel) var_sel <- NULL
      print(r$active_viz)
      if (r$active_viz == "map") {
        print("tipo de mapa")
         print(r$mapType)
        if (r$mapType  %in% c("bubbles", "heatmap")) {
          var_sel <- c("longitud", "latitud", unique(c(var_sel, "AlcaldiaHechos", "ColoniaHechos")))
        }
      }

      varAnio <- NULL
      varAdd <- "AlcaldiaHechos"
      if (r$active_viz == "line") {
        req(r$fechasId)
        print("fecha viz")
        print(r$fechasId)
        varAnio  <-  r$fechasId
        varAdd <- NULL
      }
      
      df <- df[,unique(c(var_sel, varAnio, varAdd))] %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(Víctimas = dplyr::n())
      #print(head(df))
      
     
      
      if (!(r$active_viz %in% c("map", "line"))) {
        if (!("AlcaldiaHechos" %in% var_sel)) {
          df <- df %>% dplyr::select(-AlcaldiaHechos)
        }
      }
      
      if (r$active_viz == "map") {
        if (r$mapType %in% c("choropleth")) {
          df <- df %>% dplyr::select(AlcaldiaHechos, dplyr::everything())
          if ("ColoniaHechos" %in% names(df)) {
            df <- df %>% dplyr::select(ColoniaHechos, dplyr::everything())
          }
        }
        if (r$mapType %in% c("bubbles","heatmap")) {
          #indAlc <- grep("AlcaldiaHechos|ColoniaHechos", names(df))
          df <- df %>% dplyr::group_by(ColoniaHechos) %>%
          dplyr::summarise(lon = median(longitud, na.rm = TRUE), lat = median(latitud, na.rm = TRUE), Víctimas = dplyr::n()) %>%
          dplyr::filter(lon != 0) %>% dplyr::ungroup() %>% dplyr::mutate(pctg = (Víctimas/(sum(Víctimas)))*100)
          df$label <- paste0(df$ColoniaHechos, " :", df$Víctimas)
          df <- df %>% dplyr::select(lon, lat, Víctimas, dplyr::everything())
          df$pctg <- round(df$pctg, 2)
          req(r$aggId)
          if (r$aggId == "pctg") {
           df$label <-  paste0(df$ColoniaHechos, " :", df$Víctimas, " (", df$pctg, "%)")
          }
        }
       
      }
      
      
      if (length(var_sel) == 2 & !(r$active_viz %in% c("line", "map"))) {
        if (is.null(r$axisId)) return()
        #print(r$axisId)
        if (r$axisId) {
          var_sel <- rev(var_sel)
          df <- df[,c(var_sel, "Víctimas")]
        }
      }
  
      if (any(dicVictimas$id %in% names(df))) {
        dicViz <- data.frame(id = names(df))
        dicViz <- dicViz %>% dplyr::left_join(dicVictimas)
        dicViz$label <- dplyr::coalesce(dicViz$label, dicViz$id)
        names(df) <- dicViz$label
      }
      print("ultimo df")
      print(df)
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
