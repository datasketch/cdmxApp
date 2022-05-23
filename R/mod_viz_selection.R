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
      
      viz <- c("map", "bar", "treemap", "line", "table")
      viz
      
    })
    
    
    viz_tool <- reactive({
      if (is.null(possible_viz())) return()
      df_viz <- data.frame(id = c( "map", "bar", "treemap", "line", "table"),
                           label = c("Mapa", "Barras", "Treemap", "Líneas", "Tabla"))
      df_viz <- df_viz %>% dplyr::filter(id %in% possible_viz())
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
