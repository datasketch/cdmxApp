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
    uiOutput(ns("viz_plot")),
    tags$a(href="https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj",
           "Fuente: Portal de datos abiertos de CDMX", target="_blank")
  )
}

#' load_viz Server Functions
#'
#' @noRd 
mod_load_viz_server <- function(id, r){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataViz <- reactiveValues(content = NULL)
    
    observe({
      if (is.null(r$d_viz)) return()
      if (is.null(r$active_viz)) return()
      if (r$active_viz == "map") {
        if (is.null(r$mapType)) return()
        if (r$mapType %in% "choropleth") {
          dataViz$content <- r$d_viz  
        } else {
          nsample <- nrow(r$d_viz)
          if (nsample > 7000) nsample <- 7000
          dataViz$content <- r$d_viz[sample(1:nrow(r$d_viz), nsample, replace = FALSE),] 
        }
      } else {
        dataViz$content <- r$d_viz
      }
    })
    
    
    optsViz <- reactive({
      tryCatch({
        if (is.null(r$active_viz)) return()
        if (is.null(r$colorsPlot)) return()
        if (is.null(r$colorsId)) return()
        if (is.null(dataViz$content)) return()
        df <- dataViz$content
        
        #print(head(df))
        opts_viz <- list(
          data = df,
          palette_colors = r$colorsPlot[[r$colorsId]],
          na_color = "#b8b4b3",
          hor_title = " ",
          ver_title = " ",
          orientation = "hor",
          background_color = "transparent",
          format_sample_num = "1,234.",
          title_size = 15,
          title_align = "center",
          title_color = "#141414",
          text_color = "#232323",
          #tooltip = tooltip_viz(),
          text_family = "Montserrat",
          title_family = "Montserrat",
          label_wrap = 100,
          label_wrap_legend = 100,
          marker_radius = 7,
          dataLabels_show = TRUE,
          axis_line_y_size = 1,
          axis_line_x_size = 1,
          axis_line_color = "#a7b5b3",
          #sort = "desc", ##dbd9d9 grid color
          grid_y_color = "#bcccca",
          grid_x_width = 0,
          border_weight = 0.5,
          map_zoom = 10,
          map_min_zoom = 10,
          legend_position = "topright",
          legend_layout = "vertical",
          legend_align = "right",
          legend_verticalAlign = "middle"
          
        )
        
        
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
        
        if (r$active_viz %in% c("treemap", "pie")) {
          opts_viz$color_by <- names(r$d_viz)[1]
          opts_viz$legend_show <- FALSE 
          opts_viz$palette_colors <- rev(r$colorsPlot[[r$colorsId]])
        }
        
        if (r$active_viz %in% c("map")) {
          req(r$v_sel)
          opts_viz$map_name <- "mex_mayors"
          opts_viz$palette_colors <- rev(r$colorsPlot[[r$colorsId]])
          if (r$aggId == "pctg") {
            opts_viz$format_sample_num <- "1,234.34"
            opts_viz$tooltip <- "AlcaldiaHechos: {AlcaldiaHechos} </br> Víctimas: {%}"
            if (length(r$v_sel) >= 2)  opts_viz$tooltip <- "ColoniaHechos: {ColoniaHechos}</br> Víctimas: {%}"
            opts_viz$suffix <- "%"
          }
          if (length(r$v_sel) >= 2) opts_viz$map_name <- "cdmx_colonies"
          #print(opts_viz$map_name)
          opts_viz$map_tiles <- "CartoDB.Voyager"
          opts_viz$topo_fill_opacity <- 0.7
          opts_viz$na_color <- "transparent"
          opts_viz$map_cluster <- "markerClusterOptions()"
          #opts_viz$palette_colors <- c("#7CDFEA", "#066C63")
        }
        
        if (r$active_viz %in% "bar") {
          if (r$sortBar) {
            opts_viz$sort <- "desc" 
          }
        }
        
        
        
        #print(opts_viz$agg)
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
        req(r$mapType)
        
        library(hgchmagic)
        lv <- do.call(eval(parse(text=r$v_type)), optsViz())
        
        
        lv
      },
      error = function(cond) {
        return()
      })
    })
    
    
    
    output$viz_lflt <- leaflet::renderLeaflet({
      req(viz_s())
      if (r$active_viz == "table") return()
      if (!(r$active_viz %in% c("map"))) return()
      viz_s()
    })
    
    
    observe({
      if (is.null(r$active_viz)) return()
      if (r$active_viz == "map") {
        if (is.null(r$mapType)) return()
        if (r$mapType %in% c("bubbles", "heatmap")) {
          leaflet::leafletProxy("viz_lflt") %>% 
            leaflet::setView(lng = median(dataViz$content$longitud, na.rm = TRUE),
                             lat = median(dataViz$content$latitud, na.rm = TRUE), zoom = input$viz_lflt_zoom)
        }
      }
    })
    
    output$viz_hgch <- highcharter::renderHighchart({
      req(viz_s())
      if (r$active_viz == "table") return()
      if (r$active_viz == "map") return()
      viz_s()
    })
    
    
    output$table_view <- DT::renderDataTable({
      req(r$d_fil)
      if (r$active_viz != "table") return()
      df <- r$d_fil
      df <- df %>% dplyr::select(-FechaInicioR, -FechaHechoR)
      dtable <- DT::datatable(df,
                              rownames = F,
                              options = list(
                                #autoWidth = TRUE,
                                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                #                        lengthChange = F,
                                #                        pageLength = 4,
                                scrollX = T,
                                fixedColumns = TRUE,
                                fixedHeader = TRUE,
                                scrollY = "500px"#,
                                #                        initComplete = htmlwidgets::JS(
                                #                          "function(settings, json) {",
                                #                          "$(this.api().table().header()).css({'background-color': '#a13e1f', 'color': '#fff'});",
                                #                          "}")
                                #                      )) %>%
                                # DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
                                
                              ))
      dtable
    })
    
    
    output$viz_plot <- renderUI({
      tryCatch({
        #print(r$parmesan_input)
        if (r$quest_choose != "violencia") return()
        if (r$active_viz == "table") {
          vv <- DT::dataTableOutput(ns("table_view"), width = 950)
        } else if(r$active_viz %in% c("map")) {
          vv <-leaflet::leafletOutput(ns("viz_lflt"), height = 590)  
        }else {
          vv <-highcharter::highchartOutput(ns("viz_hgch"), height = 590)
        }
        vv
      },
      error = function(cond) {
        return()
      }) 
    })
    
    
    
    
    observe({
      r$downViz <- viz_s()
      r$averZomm <- input$viz_lflt_zoom
    })
    
  })
}

## To be copied in the UI
# mod_load_viz_ui("load_viz_ui_1")

## To be copied in the server
# mod_load_viz_server("load_viz_ui_1")
