#' load_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("viz_plot"))
  )
}

#' load_viz Server Functions
#'
#' @noRd 
mod_load_viz_server <- function(id, r){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    optsViz <- reactive({
      tryCatch({
      if (is.null(r$active_viz)) return()
      opts_viz <- list(
        data = r$d_viz,
        #palette_colors = c( "#03b2ee", "#890de8", "#ff00ff", "#fcff00", "#0a7f3c", "#ff6112","#f7e2b2"),
        na_color = "#b8b4b3",
        hor_title = " ",
        ver_title = " ",
        orientation = "hor",
        background_color = "transparent",
        format_sample_num = "1,234.",
        title_size = 15,
        title_align = "center",
        title_color = "#231f20",
        text_color = "#231f20",
        #tooltip = tooltip_viz(),
        text_family = "Montserrat",
        title_family = "Montserrat",
        label_wrap = 100,
        label_wrap_legend = 100,
        marker_radius = 7,
        dataLabels_show = TRUE,
        #sort = "desc", ##dbd9d9 grid color
        grid_x_width = 0
      )
      
       if (r$active_viz %in% c("treemap", "pie")) {
          opts_viz$color_by <- names(r$d_viz)[1]
          opts_viz$legend_show <- FALSE 
       }
      
      if (r$active_viz %in% c("choropleth", "bubbles")) {
        opts_viz$map_name <- "mex_mayors"
      }
      
      # if (r$active_viz %in% "bubbles") {
      #   opts_viz$map_name <- "mex_states"
      # }
      
      opts_viz$agg <- "sum"
      opts_viz$percentage <- FALSE
      opts_viz$graph_type <- "grouped"
      if (r$aggId == "pctg") {
        opts_viz$percentage <- TRUE 
      } else {
        opts_viz$agg <- r$aggId
      }
      
      if (!is.null(r$stackedId)) {
      if (r$stackedId) {
        opts_viz$graph_type <-  "stacked"
       }
      }
      
      opts_viz
      },
      error = function(cond) {
        return()
      })
    })
    
    viz_s <- reactive({
      tryCatch({
      if (r$active_viz == "table") return()
      req(optsViz())
      req(r$v_type)
      req(r$d_viz)
      print("VEEEER")
      print(optsViz()$data)
      print(optsViz()$map_name)
      print(r$v_type)
        # if (r$active_viz %in% c("choropleth", "bubbles")) {
      #   do.call(eval(parse(text=r$v_type)), optsViz())
      # } else {
      library(hgchmagic)
      do.call(eval(parse(text=r$v_type)), optsViz())
      #}
      },
      error = function(cond) {
        return()
      })
    })

    output$viz_lflt <- leaflet::renderLeaflet({
      req(viz_s())
      if (r$active_viz == "table") return()
      if (!(r$active_viz %in% c("choropleth", "bubbles"))) return()
      viz_s()
    })
    
    output$viz_hgch <- highcharter::renderHighchart({
      req(viz_s())
      if (r$active_viz == "table") return()
      if (r$active_viz == "choropleth") return()
      if (r$active_viz == "bubbles") return()
      viz_s()
    })
    
    
    output$table_view <- DT::renderDataTable({
      req(r$d_fil)
      if (r$active_viz != "table") return()
      df <- r$d_fil
      
      dtable <- DT::datatable(df,
                             rownames = F,
                             escape = FALSE,
                             options = list(
                               autoWidth = TRUE,
                               scrollX = TRUE,   ## enable scrolling on X axis
                               scrollY = TRUE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                               lengthChange = F,
                               pageLength = 4,
                               scrollX = T,
                               scrollY = T,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#a13e1f', 'color': '#fff'});",
                                 "}")
                             )) %>%
        DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
      
      
      dtable
    })
    
    
    output$viz_plot <- renderUI({
      tryCatch({
      if (r$quest_choose != "violencia") return()
        div(
      if (r$active_viz == "table") {
        DT::dataTableOutput(ns("table_view"), width = 980)
      } else if(r$active_viz %in% c("choropleth", "bubbles")) {
      leaflet::leafletOutput(ns("viz_lflt"), height = 550)  
      }else {
      highcharter::highchartOutput(ns("viz_hgch"), height = 550)
      },
      tags$a(href="https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj",
             "Fuente: Portal de datos abiertos de CDMX", target="_blank")
      )
      },
      error = function(cond) {
        return()
      }) 
    })
    
    
    observe(
      r$downViz <- viz_s()
    )
    
  })
}

## To be copied in the UI
# mod_load_viz_ui("load_viz_ui_1")

## To be copied in the server
# mod_load_viz_server("load_viz_ui_1")
