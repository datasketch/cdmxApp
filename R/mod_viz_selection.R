#' viz_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("viz_icons"))
  )
}

#' viz_selection Server Functions
#'
#' @noRd 
mod_viz_selection_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    possible_viz <- reactive({
    
      reqViz <- NULL
      viz <- c( "map", "map_heat", "map_bubbles",  "bar", "treemap", "line", "scatter")
      if (!is.null(r$ckanConf$chart_type)) {
        #reqViz <- setdiff(listConf$result$chart_type %>% stringr::str_split(pattern = ",") %>% .[[1]], c(NA, ""))
        reqViz <-  trimws(setdiff(r$ckanConf$chart_type %>% 
                                    stringr::str_split(pattern = ",") %>% 
                                    .[[1]], 
                                  c(NA, "", " ")))
        if (!identical(reqViz, character())) {
          viz <- unique(c(reqViz, viz))
        }
      }
      if (is.null(r$allCats)) viz <- setdiff(viz, c("bar", "treemap"))
      if (is.null(r$allNums)) viz <- setdiff(viz, "scatter")
      if (!is.null(r$allNums)) {
        if (length(r$allNums) < 2) viz <- setdiff(viz, "scatter")
      }
      if (is.null(r$allDates)) viz <- setdiff(viz, "line")
      if (is.null(r$coorToPlot)) viz <- setdiff(viz, c( "map_heat", "map_bubbles"))
      if (is.null(r$geoToPlot)) viz <- setdiff(viz, "map")
      if (!any(grepl("alcaldia|alcaldía", tolower(r$vars_f$vars)))) viz <- setdiff(viz, c("map"))
      

      c(viz, "table")
      
      
      
    })
    
    
    viz_tool <- reactive({
    
      if (is.null(possible_viz())) return()
      df_viz <- data.frame(id = c( "map", "map_heat" ,"map_bubbles", "bar", "treemap", "line", "scatter"  ,"table"),
                           label = c( "Coropleta", "Mapa de calor", "Puntos" ,"Barras","Treemap", "Líneas", "Dispersión", "Tabla"))
      viz_a <- data.frame(id = possible_viz())
      df_viz <- viz_a %>% dplyr::left_join(df_viz) %>% dplyr::filter(id %in% possible_viz())

      df_viz$label
    })
    
    actual_but <- reactiveValues(active = NULL)
    
    observe({
      if (is.null(input$viz_selection)) return()
      viz_rec <- possible_viz()
      if (input$viz_selection %in% viz_rec) {
        actual_but$active <- input$viz_selection
      } else {
        actual_but$active <- viz_rec[1]
      }
    })
    
    
    # print viz
    output$viz_icons <- renderUI({
      possible_viz <- possible_viz()
      #print(app_sys("app/www/viz_icons/"))
      suppressWarnings(
        shinyinvoer::buttonImageInput(ns('viz_selection'),
                                      " ",#div(class="title-data-select", "Selecciona tipo de visualización"),
                                      images = possible_viz,
                                      path = app_sys("app/www/viz_icons/"),#app_sys(paste0("app/www/viz_icons/", "reconocimientoFacialApp")),
                                      active = actual_but$active,
                                      tooltips = viz_tool(),
                                      imageStyle = list(borderColor = "#ffffff",
                                                        borderSize = "1px",
                                                        padding = "7px",
                                                        shadow = TRUE)
        )
      )
    })
    
    
    observe({
      r$active_viz <- actual_but$active
    })
    
    
  })
}

## To be copied in the UI
# mod_viz_selection_ui("viz_selection_ui_1")

## To be copied in the server
# mod_viz_selection_server("viz_selection_ui_1")
