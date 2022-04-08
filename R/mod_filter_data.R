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
      
      
      dataFilter <- reactiveValues(info = NULL)
      
      observe({
         tryCatch({
            qs <- r$quest_choose
            
            if (qs != "violencia") return()
            
            req(r$d_sel)
            df <- r$d_sel
            vars_f <- r$vars_f
            
            for (i in 1:nrow(vars_f)) {
               if (is.null(r[[vars_f$id[i]]])) {
                  naInd <- is.na(df[[vars_f$vars[i]]])
                  if (any(naInd)) {
                     df <- df[naInd,]
                  }
               } else if (r[[vars_f$id[i]]] == "Todas") {
                   df <- df 
               } else {
                  if (!all(r$allCats[[vars_f$vars[i]]] %in% r[[vars_f$id[i]]])) {
                     catInd <- grep(paste0(r[[vars_f$id[i]]], collapse = "|"), df[[vars_f$vars[i]]])
                     df <- df[catInd,]
                  }
               }
            }   
            df$FechaInicio <- lubridate::dmy(df$FechaInicio)
            if (!is.null(r$anioId)) {
               cambioEdad <- !all(df$FechaInicio %in% r$anioId)
               if (cambioEdad) {
                  if (length(r$anioId) == 1) {
                     df <- df %>% dplyr::filter(FechaInicioR %in% format(r$anioId, format="%Y-%m"))
                  } else {
                     print(format(r$anioId[1], format="%Y-%m"))
                     df <- df %>% dplyr::filter(FechaInicioR >= format(r$anioId[1], format="%Y-%m") & FechaInicioR <= format(r$anioId[2], format="%Y-%m"))
                  }
               }
            }
            if (is.null(df) | nrow(df) == 0) return()
            dataFilter$info <- df
         },
         error = function(cond) {
            return()
         })
         
         
      })
      
      
      
      


      varSelection <- reactiveValues(id = NULL)
      observe({
         if (is.null(r$active_viz)) return()
         if (r$active_viz %in% c("bar", "treemap", "map")) {
            if (is.null(r$varViewId)) return()
            if (is.null(r$desagregacionId)) return()
            varAdd <- r$desagregacionId
            if (varAdd == "cdmx") return()
            if (r$desagregacionId == "ninguna") varAdd <- NULL
            varSelection$id <- c(varAdd, r$varViewId)
         } else if (r$active_viz %in% c( "line")) {
            if (is.null(r$varViewId)) return()
            varSelection$id <- r$varViewId
         } else {
            return()
            #if (is.null(r$varOtherId)) return()
            #varSelection$id <- "AlcaldiaHechos"
         }

      })

      data_summary <- reactive({
         req(dataFilter$info)
         df <- dataFilter$info %>% dplyr::group_by(Sexo) %>% dplyr::summarise(Total = dplyr::n())
         df
      })

      observe({
         r$d_fil <- dataFilter$info
         r$v_sel <- varSelection$id
         r$d_sum <- data_summary()
      })
      
      
   })
}

## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")

## To be copied in the server
# mod_filter_data_server("filter_data_ui_1")
