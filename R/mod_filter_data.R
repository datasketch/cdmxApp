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
        req(r$d_sel)
        df <- r$d_sel
        vars_f <- r$vars_f
        for (i in 1:nrow(vars_f)) {
          if (is.null(r[[vars_f$id[i]]])) df <- df
          if (!any(r[[vars_f$id[i]]] %in% "Todas")) {

            filterNA <- FALSE
            if (is.null(r[[vars_f$id[i]]]) | is.na(r[[vars_f$id[i]]])) filterNA <- TRUE

            if (!all(r$allCats[[vars_f$vars[i]]] %in% r[[vars_f$id[i]]])) {
              df <- df %>% filterTbl(varToFilter = vars_f$vars[i], catsToView = r[[vars_f$id[i]]], filterNA = filterNA)
            }

          }
        }
        # 
        # df$FechaInicio <- lubridate::dmy(df$FechaInicio)
        # if (!is.null(r$anioId)) {
        #   cambioEdad <- !(min(df$FechaInicio, na.rm = T) == r$anioId[1] & max(df$FechaInicio, na.rm = T) == r$anioId[2])
        #   # print(r$anioId)
        #   # print(cambioEdad)
        #   if (cambioEdad) {
        #     if (length(r$anioId) == 1) {
        #       df <- df %>% dplyr::filter(FechaInicioR %in% format(r$anioId, format="%Y-%m"))
        #     } else {
        #       print(format(r$anioId[1], format="%Y-%m"))
        #       df <- df %>% dplyr::filter(FechaInicioR >= format(r$anioId[1], format="%Y-%m") & FechaInicioR <= format(r$anioId[2], format="%Y-%m"))
        #     }
        #   }
        # }
        # if (is.null(df) | nrow(df) == 0) return()
        # print(nrow(df))
        dataFilter$info <- df
      },
      error = function(cond) {
        return()
      })


    })






    # varSelection <- reactiveValues(id = NULL)
    # observe({
    #   tryCatch({
    #     if (is.null(r$active_viz)) return()
    #     if (r$active_viz %in% c("bar", "treemap", "map", "map_bubbles")) {
    #       if (is.null(r$varViewId)) return()
    #       if (is.null(r$desagregacionId)) return()
    #       varAdd <- r$desagregacionId
    #       if (varAdd == "cdmx") return()
    #       if (r$desagregacionId == "ninguna") varAdd <- NULL
    #       varSelection$id <- c(varAdd, r$varViewId)
    #       if (r$active_viz == "treemap") varSelection$id <- c(r$varViewId, varAdd)
    #     } else if (r$active_viz %in% c( "line")) {
    #       if (is.null(r$varViewId)) return()
    #       varSelection$id <- r$varViewId
    #     } else {
    #       return()
    #       #if (is.null(r$varOtherId)) return()
    #       #varSelection$id <- "AlcaldiaHechos"
    #     }
    #   },
    #   error = function(cond) {
    #     return()
    #   })
    # })

    data_summary <- reactive({
      tryCatch({
        req(dataFilter$info)
        df <- dataFilter$info %>% dplyr::summarise(Total = dplyr::n()) %>% dplyr::collect()
        df},
        error = function(cond) {
          return()
        })
    })

    observe({
      r$d_fil <- dataFilter$info
      #r$v_sel <- varSelection$id
      r$d_sum <- data_summary()
    })
    
    
  })
}

## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")

## To be copied in the server
# mod_filter_data_server("filter_data_ui_1")
