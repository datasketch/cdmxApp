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
    #verbatimTextOutput(ns("testData"))
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
    
    
    # output$testData <- renderPrint({
    #   r$d_viz
    # })
    # 
    
    
    output$map1 <- leaflet::renderLeaflet({
      #tryCatch({
      if (!(r$active_viz %in% c("map_bubbles", "map_heat"))) return()
      
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
      
      
      if (grepl("colonia", tolower(r$desagregacionId))) {
        lm <- lm %>%
          leaflet::addTopoJSON(topojson = coloniaCdmx,
                               weight = 1, opacity = 0.5,
                               fillColor = "transparent",
                               color = "#000000")
      }
      
      lm <- lm %>%
        leaflet::setView(lng = -99.2, lat = 19.33, 10.70) 
      lm
    })
    
    dataMap <- reactiveValues(info = NULL)
    observe({
       tryCatch({
        if (is.null(input$map1_zoom)) return()
        if (is.null(r$d_viz)) return()
        if (!(r$active_viz %in% c("map_bubbles", "map_heat"))) return()
        df <- r$d_viz %>% tidyr::drop_na()
        
        if (input$map1_zoom <= 10) {
          nsample <- 10000
          if (nrow(df) < 10000) nsample <- nrow(df)
          options(set.seed(999))
          df <- df[sample(1:nrow(df), nsample, replace = FALSE),]
        } else {
          if (nrow(df) < 10000) {
            nsample <- nrow(df)
          } else {
            options(set.seed(999))
            fac_mul <- nrow(df)/1000
            nsample <- round((input$map1_zoom*1000) + fac_mul)
          }
          df <- df[sample(1:nrow(df), nsample, replace = FALSE),]
        }
        names(df) <- tolower(names(df))
    
        lf <- leaflet::leafletProxy("map1", data = df)
        if (r$active_viz == "map_bubbles") {
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
        } else {
          print("en heatmap")
          print(df)
          lf <- lf %>%
            leaflet.extras::clearHeatmap() %>%
            leaflet.extras::addHeatmap(
              lng = ~longitud,
              lat = ~latitud,
              #label = ~label,
              radius = 8#,
              #blur = 45,
              #minOpacity = 0.5,
              #gradient="GnYlRd"
            )
        }
        if (nrow(r$d_viz) > nrow(df)) {
        lf <-
          lf %>%
          leaflet::clearControls() %>%
          leaflet::addControl(paste0("Este mapa solo muestra ", nrow(df), " puntos a la vez. Da zoom para ver todos los puntos a nivel calle o colonia"),
                              position = "bottomleft", className = "map-caption")
        } else {
          lf <-
            lf %>%
            leaflet::clearControls()
        }
        lf
      },
      error = function(cond) {
        return()
      })
    })
    
    
    optsViz <- reactive({
      
      tryCatch({
        if (is.null(r$active_viz)) return()
        if (r$active_viz == "map_bubbles") return()
        if (r$active_viz == "map_heat") return()
        if (is.null(r$colorsPlot)) return()
        if (is.null(r$colorsId)) return()
        if (is.null(r$d_viz)) return()
        df <- r$d_viz
        # if (any(c("lon", "longitud") %in% names(df))) return()
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
          legend_backgroundWidth = 1,
          legend_backgroundBorderColor = "#cccccc",
          map_canvas = TRUE,
          map_min_size = 3,
          map_max_size = 10,
          treemap_borderWidth_levelOne = 3,
          treemap_borderColor_levelOne = "#232323"
          # map_tiles_zoom_update = TRUE,
          # map_tiles_id_update = FALSE
        )
        
        #print(df)
        if (r$aggId == "pctg") {
          opts_viz$percentage <- TRUE
          if (ncol(df) > 2) opts_viz$percentage_by <- names(df)[[2]]
        }
        if (!is.null(r$pctgNum)) {
          if (r$pctgNum) {
            opts_viz$percentage <- TRUE
            if (ncol(df) > 2) opts_viz$percentage_by <- names(df)[[2]]
          }
        }
        #print(opts_viz$percentage_by)
        
        if (!is.null(r$stackedId)) {
          if (r$stackedId) {
            opts_viz$graph_type <-  "stacked"
          }
        }
        
        if (r$active_viz %in% c("treemap", "pie", "scatter")) {
          opts_viz$color_by <- names(r$d_viz)[1]
          opts_viz$legend_show <- FALSE
          opts_viz$palette_colors <- rev(r$colorsPlot[[r$colorsId]])
        }
        
        if (r$active_viz %in% c("bar", "line")) {
          opts_viz$legend_title <- '<span style="font-size: 9px; color: #666; font-weight: normal">Haz clic en la leyenda para mostrar u </br>ocultar la categoría</span>'
        }
        
        
        if (r$active_viz %in% c("map")) {
          req(r$v_sel)
          opts_viz$map_name <- "mex_mayors"
          opts_viz$legend_title <- " "
          opts_viz$palette_colors <- rev(r$colorsPlot[[r$colorsId]])
          if (r$aggId == "pctg") {
            opts_viz$format_sample_num <- "1,234.34"
            #opts_viz$tooltip <- "AlcaldiaHechos: {Alcaldía} </br> Víctimas: {%}"
            #if (length(r$v_sel) >= 2)  opts_viz$tooltip <- "Colonia: {Colonia}</br> Víctimas: {%}"
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
        if (r$active_viz == "scatter") {
          if (r$aggScatterViz) {
            opts_viz$scatter_agg <- TRUE
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
        if (is.null(r$active_viz)) return()
        if (r$active_viz == "table") return()
        #  if (r$active_viz %in% c("map", "map_heat", "map_bubbles")) return()
        req(optsViz())
        req(r$v_type)
        req(r$d_viz)
        library(hgchmagic)
        print(r$v_type)
        lv <- do.call(eval(parse(text=r$v_type)), optsViz())
        
        #print(lv)
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
        if (r$active_viz == "map_heat") return()
        viz_s()
      },
      error = function(cond) {
        return()
      })
    })
    
    
    output$table_view <- DT::renderDataTable({
      req(r$d_fil)
      if (r$active_viz != "table") return()
      df <- r$d_fil %>% dplyr::collect()
      #df <- df %>% dplyr::select(-FechaInicioR, -FechaHechoR)
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
        if (r$active_viz == "table") {
          vv <- shinycustomloader::withLoader(ui_element = DT::dataTableOutput(ns("table_view"), width = 950), type = "html", loader = "loader4")
        } else if (r$active_viz %in% c("map")) {
          vv <- shinycustomloader::withLoader(ui_element = leaflet::leafletOutput(ns("viz_lflt"), height = 630), type = "html", loader = "loader4")
        } else if (r$active_viz %in% c("map_bubbles", "map_heat")) {
          vv <- shinycustomloader::withLoader(ui_element = leaflet::leafletOutput(ns("map1"), height = 630), type = "html", loader = "loader4")
        } else {
          vv <- shinycustomloader::withLoader(ui_element = highcharter::highchartOutput(ns("viz_hgch"), height = 630), type = "html", loader = "loader4")
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
