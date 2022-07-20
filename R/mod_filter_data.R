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
        if (!is.null(r$vars_f)) {
          vars_f <- r$vars_f
          for (i in 1:nrow(vars_f)) {
            if (is.null(r[[vars_f$id[i]]])) return()
            filterNA <- "NA" %in% r[[vars_f$id[i]]]
            varF <- setdiff(r[[vars_f$id[i]]], "NA")
            df <- filterTbl(df, varToFilter = vars_f$vars[i], catsToView = varF, filterNA = filterNA)
          }
        } 
        #print(df)
        print(r$allNums)
        print(r$numRange)
        if (!is.null(r$allNums)) {
          if (!is.null(r$numRange)) {
            for (i in r$allNums) {
              rangeDef <- r$numRange %>% dplyr::filter(id %in% i)
              df <- filterNumTbl(dataTbl = df,
                                 varToFilter = i,
                                 rangeToView = r[[paste0(i, "range")]],
                                 originalRange = c(rangeDef$min, rangeDef$max))
            }
          }
        }
        
        if (!is.null(r$allDates)) {
          print(r$allDates)
          rangeDef <- r$datesRange[1,]
          rangeDef$dateFil <- paste0("temporal_",rangeDef$id)
          print(c(format(as.Date(r[[paste0(rangeDef$id, "range")]][1]), format="%Y-%m"),format(as.Date(r[[paste0(rangeDef$id, "range")]][2]), format="%Y-%m")))
          print(c(format(as.Date(rangeDef$min), format="%Y-%m"), format(as.Date(rangeDef$max))))
          df <- filterNumTbl(df,
                             rangeDef$dateFil,
                             c(format(as.Date(r[[paste0(rangeDef$id, "range")]][1]), format="%Y-%m"),format(as.Date(r[[paste0(rangeDef$id, "range")]][2]), format="%Y-%m")),
                             c(format(as.Date(rangeDef$min), format="%Y-%m"), format(as.Date(rangeDef$max), format="%Y-%m"))
          )
        }
        
        dataFilter$info <- df
      },
      error = function(cond) {
        return()
      })
      
      
    })
    
    
    
    
    
    
    
    
    varSelection <- reactiveValues(id = NULL)
    observe({
      tryCatch({
        if (is.null(r$active_viz)) return()
        if (r$active_viz %in% c("bar", "treemap", "map", "map_bubbles")) {
          if (is.null(r$varViewId)) return()
          if (is.null(r$desagregacionId)) return()
          varAdd <- r$desagregacionId
          #if (varAdd == "Histórico CDMX") return()
          if (r$desagregacionId == "ninguna") varAdd <- NULL
          varSelection$id <- c(varAdd, r$varViewId)
          if (r$active_viz == "treemap") varSelection$id <- c(r$varViewId, varAdd)
        } else if (r$active_viz %in% c( "line", "scatter")) {
          if (is.null(r$varViewId)) return()
          varSelection$id <- r$varViewId
        } else {
          return()
        }
      },
      error = function(cond) {
        return()
      })
    })
    
    data_summary <- reactive({
      tryCatch({
        req(dataFilter$info)
        df <- dataFilter$info %>% dplyr::summarise(Total = dplyr::n()) %>% dplyr::collect()
        df
      },
      error = function(cond) {
        return()
      })
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