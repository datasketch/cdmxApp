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
    div(style="display: flex;justify-content: space-between;",
        tags$a(href="https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj",
               "Fuente: Portal de datos abiertos de CDMX", target="_blank"),
        uiOutput(ns("logos_add"))
    )
  )
}

#' load_viz Server Functions
#'
#' @noRd 
mod_load_viz_server <- function(id, r){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$map1 <- leaflet::renderLeaflet({
      #tryCatch({
      if (r$active_viz != "map_bubbles") return()
      
      req(r$desagregacionId)
      lm <-  leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.5, 
                                                                zoomDelta = 0.5
      )) %>% 
        leaflet::addTiles(urlTemplate = "https://maps.geoapify.com/v1/tile/positron/{z}/{x}/{y}.png?&apiKey=f39345000acd4188aae1f2f4eed3ff14",
                          attribution = "positron") %>% 
        leaflet::addTopoJSON(topojson = mayorsCdmx, 
                             weight = 1, opacity = 0.5, 
                             fillColor = "transparent",
                             color = "#000000")  
      
      
      if (r$desagregacionId == "ColoniaHechos") {
        lm <- lm %>% 
          leaflet::addTopoJSON(topojson = coloniaCdmx, 
                               weight = 1, opacity = 0.5, 
                               fillColor = "transparent",
                               color = "#000000")  
      }
      
      lm <- lm %>% 
        leaflet::setView(lng = -99.2, lat = 19.33, 10.70) %>% 
        leaflet::addControl("Este mapa solo muestra 15,000 puntos a la vez. Da zoom para ver todos los puntos a nivel calle o colonia", 
                            position = "bottomleft", className = "map-caption")
      lm
    })
    
    
    dataMap <- reactive({
      tryCatch({
        if (is.null(r$d_viz)) return()
        if (r$active_viz != "map_bubbles") return()
        df <- r$d_viz
        if (input$map1_zoom <= 10) {
          nsample <- 10000
          if (nrow(df) < 10000) nsample <- nrow(df)
          df <- df[sample(1:nrow(df), nsample, replace = FALSE),]
        } else {
          nsample <- (input$map1_zoom*1000) + 15000
          if (nsample > nrow(df)) nsample <- nrow(df)
          df <- df[sample(1:nrow(df), nsample, replace = FALSE),]
        }
        df
        },
        error = function(cond) {
          return()
        })
    })
    
    
    observe({
      req(r$active_viz)
      if (r$active_viz != "map_bubbles") return()
      tryCatch({
        req(dataMap())
        df <- dataMap()
        lf <- leaflet::leafletProxy("map1", data = df)

        lf <- lf %>%
         leaflet::clearMarkerClusters() %>% 
          leaflet::addCircleMarkers(
            lng = ~longitud,
            lat = ~latitud,
            #label = ~label,
            radius = 5,
            color = "#19719F",
            clusterOptions = leaflet::markerClusterOptions(
              maxClusterRadius = 50,
              showCoverageOnHover = TRUE,
              spiderLegPolylineOptions = list(weight = 0),
              zoomToBoundsOnClick = TRUE,
              spiderfyOnMaxZoom = TRUE,
              removeOutsideVisibleBounds = TRUE
            )
          ) #%>% 
      },
      error = function(cond) {
        return()
      })
    })
    
    
    optsViz <- reactive({
      tryCatch({
        if (is.null(r$active_viz)) return()
        if (r$active_viz == "map_bubbles") return()
        if (is.null(r$colorsPlot)) return()
        if (is.null(r$colorsId)) return()
        if (is.null(r$d_viz)) return()
        df <- r$d_viz
     
        if (any(c("lon", "longitud") %in% names(df))) return()
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
          border_weight = 0.2,
          map_zoom_snap = 0.25,
          map_zoom_delta = 0.25,
          #map_zoom = 11,
          map_min_zoom = 10,
          map_max_zoom = 14,
          legend_position = "topright",
          legend_layout = "vertical",
          legend_align = "right",
          legend_verticalAlign = "middle",
          map_canvas = TRUE,
          map_min_size = 3,
          map_max_size = 10,
          treemap_borderWidth_levelOne = 3,
          treemap_borderColor_levelOne = "#232323"
          # map_tiles_zoom_update = TRUE,
          # map_tiles_id_update = FALSE
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
            opts_viz$tooltip <- "AlcaldiaHechos: {Alcaldía} </br> Víctimas: {%}"
            if (length(r$v_sel) >= 2)  opts_viz$tooltip <- "Colonia: {Colonia}</br> Víctimas: {%}"
            opts_viz$suffix <- "%"
          }
          if (length(r$v_sel) >= 2) {
            opts_viz$map_name <- "cdmx_colonies"
            opts_viz$map_extra_layer <- TRUE
            opts_viz$map_name_extra <- "mex_mayors"
          }
          opts_viz$map_provider_tile = "url"
          opts_viz$map_extra_layout = "https://maps.geoapify.com/v1/tile/positron/{z}/{x}/{y}.png?&apiKey=f39345000acd4188aae1f2f4eed3ff14"
          opts_viz$map_name_layout = "positron"
          opts_viz$topo_fill_opacity <- 0.6
          opts_viz$max_topo_fill_opacity <- 0.8
          opts_viz$map_opacity <- 0.5
          opts_viz$na_color <- "transparent"
          #opts_viz$map_cluster <- "markerClusterOptions(maxClusterRadius = 20)"
          #opts_viz$palette_colors <- c("#7CDFEA", "#066C63")
        }
        
        if (r$active_viz %in% "bar") {
          if (r$sortBar) {
            opts_viz$sort <- "desc" 
          }
        }
        
        
        
        #print(opts_viz)
        opts_viz
      },
      error = function(cond) {
        return()
      })
    })
    
    viz_s <- reactive({
      
      tryCatch({
        if (r$active_viz == "table") return()
        if (r$active_viz == "map_bubbles") return()
        req(optsViz())
        req(r$v_type)
        req(r$d_viz)
        
        library(hgchmagic)
        
        lv <- do.call(eval(parse(text=r$v_type)), optsViz())
        
        
        lv
      },
      error = function(cond) {
        return()
      })
    })
    
    
    
    output$viz_lflt <- leaflet::renderLeaflet({
      tryCatch({      
        req(viz_s())
        if (r$active_viz != "map") return()
        viz_s() %>% 
          leaflet::setView(lng = -99.2, lat = 19.33, 10.70)
      },
      error = function(cond) {
        return()
      })
    })
    
    
    output$viz_hgch <- highcharter::renderHighchart({
      tryCatch({
        req(viz_s())
        if (r$active_viz == "table") return()
        if (r$active_viz == "map") return()
        if (r$active_viz == "map_bubbles") return()
        viz_s()
      },
      error = function(cond) {
        return()
      })
    })
    
    
    output$table_view <- DT::renderDataTable({
      req(r$d_fil)
      if (r$active_viz != "table") return()
      df <- r$d_fil
      df <- df %>% dplyr::select(-FechaInicioR, -FechaHechoR)
      dtable <- DT::datatable(df,
                              rownames = F,
                              selection = 'none',
                              options = list(
                                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                scrollX = T,
                                fixedColumns = TRUE,
                                fixedHeader = TRUE,
                                scrollY = "500px"
                              ))
      dtable
    })
    
    
    output$viz_plot <- renderUI({
      tryCatch({
        #print(r$parmesan_input)
        if (r$quest_choose != "violencia") return()
        if (r$active_viz == "table") {
          vv <- DT::dataTableOutput(ns("table_view"), width = 950)
        } else if (r$active_viz %in% c("map")) {
          vv <- leaflet::leafletOutput(ns("viz_lflt"), height = 630) 
        } else if (r$active_viz %in% "map_bubbles") {
          vv <- leaflet::leafletOutput(ns("map1"), height = 630)
        } else {
          vv <-highcharter::highchartOutput(ns("viz_hgch"), height = 630)
        }
        vv
      },
      error = function(cond) {
        return()
      })
    })
    
    
    output$logos_add <- renderUI({
      HTML(
        paste0(
          '<div style="display:flex; align-items: baseline;">
        <img src="www/img/licence.svg">
        <img src="www/img/creative.svg">
        <img src="www/img/open.svg"></div>')
      )
    })
    
    
    observe({
      r$downViz <- viz_s()
    })
    
  })
}

## To be copied in the UI
# mod_load_viz_ui("load_viz_ui_1")

## To be copied in the server
# mod_load_viz_server("load_viz_ui_1")
