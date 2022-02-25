#' filter_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' filter_data Server Functions
#'
#' @noRd 
mod_filter_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    
   data_filter <- reactive({
     tryCatch({
     qs <- r$quest_choose
     
     if (qs != "violencia") return()
     
     req(r$d_sel)
     req(r$categoriaId)
     req(r$calidadId)
     req(r$alcaldiasId)
     df <- r$d_sel
     
     
     if (r$categoriaId != "TODAS") {
       df <- df %>% dplyr::filter(Categoria %in% r$categoriaId)
     }
     if (r$calidadId != "TODAS") {
       df <- df %>% dplyr::filter(CalidadJuridica %in% r$calidadId)
     }
     cambioEdad <- !all(c(1917, 2022) %in% r$anioId)
       if (cambioEdad) {
         if (length(r$anioId) == 1) {
           df <- df %>% dplyr::filter(Año_hecho == r$anioId)
         } else {
           i_Edad <- r$anioId[1]:r$anioId[2]
           df <- df %>% dplyr::filter(Año_hecho %in% i_Edad)
         }
       }
     print(r$active_viz)
     print(r$alcaldiasId )
     if (r$active_viz != "bubbles") {
     if (r$alcaldiasId == "CDMX") {
        idAlc <- alcaldiasCdmx %>% dplyr::filter(idAlcaldias == "CDMX ALCALDÍAS")
        df <- df %>% dplyr::filter(AlcaldiaHechos %in% idAlc$AlcaldiaHechos)
     }
     if (!(r$alcaldiasId %in% c("TODAS", "CDMX"))) {
        df <- df %>% dplyr::filter(AlcaldiaHechos %in% r$alcaldiasId)
     }
     } else {
        if (r$alcaldiasId  %in% c("TODAS", "CDMX")) return()
        df <- df %>% dplyr::filter(AlcaldiaHechos %in% r$alcaldiasId)
     }
     #print(head(df))
     df
     },
     error = function(cond) {
       return()
     })
     
     
   })
   
   

   
   
   
   varSelection <- reactiveValues(id = "AlcaldiaHechos")
   observe({
     if (is.null(r$active_viz)) return()
     if (r$active_viz %in% c("bar", "treemap")) {
       if (is.null(r$varViewId)) return()
       varSelection$id <- r$varViewId
     }
     if (r$active_viz %in% c("line", "pie", "choropleth", "bubbles")) {
       if (is.null(r$varOtherId)) return()
       varSelection$id <- r$varOtherId
     }
     
   })
   
   
   observe({
     r$d_fil <- data_filter()
     r$v_sel <- varSelection$id
   })
   
    
  })
}
    
## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")
    
## To be copied in the server
# mod_filter_data_server("filter_data_ui_1")
