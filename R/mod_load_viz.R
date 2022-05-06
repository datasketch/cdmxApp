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
    
    
    dataMap <- reactive({
      if (is.null(r$d_viz)) return()
      if (r$active_viz %in% c("map")) {
        req(r$mapType )
        if (r$mapType == "choropleth") return()
      }
      df <- r$d_viz
      df
    })
    
    output$map1 <- leaflet::renderLeaflet({
      tryCatch({
      if (r$active_viz %in% c("map")) {
        req(r$mapType )
        if (r$mapType == "choropleth") return()
      }
      
        lm <-
      leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.5, 
                                                         zoomDelta = 0.5
      )) %>% 
        leaflet::addProviderTiles("CartoDB.Voyager") %>% 
        leaflet::addTopoJSON(topojson = mayorsCdmx, 
                             weight = 1, opacity = 0.5, 
                             fillColor = "transparent",
                             color = "#000000")  %>% 
        leaflet::setView(lng = -99.2, lat = 19.3, 11)
          lm
      },
      error = function(cond) {
        return()
      })
    })
    
  
    
    tryCatch({
    observe({
      req(r$active_viz)
      if (r$active_viz != "map") return()
      if (r$active_viz %in% c("map")) {
        req(r$mapType )
        if (r$mapType == "choropleth") return()
      }
      if (is.null(input$map1_zoom)) return()
      req(dataMap())
      lf <- leaflet::leafletProxy("map1", data = dataMap()) #%>% 
        #leaflet::clearMarkers() 
   
      lf <- lf %>%  
        # leaflet::removeMarkerCluster(layerId = ~Colonia) %>% 
        # leaflet::removeMarker(layerId = ~Colonia) %>% 
        leaflet::addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          label = ~label,
          options = leaflet::markerOptions(victimas = ~Víctimas),
          clusterOptions = leaflet::markerClusterOptions(
            iconCreateFunction=JS("function (cluster) {    
    var markers = cluster.getAllChildMarkers();
    var sum = 0; 
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.victimas);
    }
    if(!sum)  return undefined;
    return new L.DivIcon({ html: '<div><span>' + sum + '</span></div>', className: 'marker-cluster marker-cluster-medium', iconSize: new L.Point(40,40)});
  }"),  maxClusterRadius = 100,
            showCoverageOnHover = TRUE,
            spiderfyDistanceMultiplier = 1.5,
            spiderLegPolylineOptions = list(weight = 0),
            #zoomToBoundsOnClick = TRUE,
            spiderfyOnMaxZoom = TRUE,
            removeOutsideVisibleBounds = TRUE
          )
        )
    })
    },
    error = function(cond) {
      return()
    })
    
    optsViz <- reactive({
      tryCatch({
        if (is.null(r$active_viz)) return()
        if (is.null(r$colorsPlot)) return()
        if (is.null(r$colorsId)) return()
        if (is.null(r$d_viz)) return()
        if (r$active_viz %in% c("map")) {
          req(r$mapType )
          if (r$mapType %in% c("bubbles", "heatmap")) return()
        }
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
          map_max_size = 10
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
          opts_viz$map_tiles <- "CartoDB.Voyager"
          opts_viz$fill_opacity <- 0.3
          opts_viz$na_color <- "transparent"
          #opts_viz$map_cluster <- "markerClusterOptions(maxClusterRadius = 20)"
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
        if (r$active_viz == "table") return()
        if (!(r$active_viz %in% c("map"))) return()
        req(r$mapType )
        if (r$mapType == "bubbles") return()
        
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
          req(r$mapType )
          if (r$mapType == "bubbles") {
            vv <-  leaflet::leafletOutput(ns("map1"), height = 630) 
          } else {
            vv <- leaflet::leafletOutput(ns("viz_lflt"), height = 630) 
          }
        }else {
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
