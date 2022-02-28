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
    verbatimTextOutput(ns("aver")),
    div(style = "display:flex;align-items: center;justify-content: space-between;gap:15px;",
    uiOutput(ns("summaryInfo")),
    uiOutput(ns("infoButt"))
    )
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
      if (is.null(r$colorsId)) return()
      opts_viz <- list(
        data = r$d_viz,
        palette_colors = r$colorsId,#c("#066c63", "#39d361", "#f9e928", "#0a87cc", "#eed7ba", "#d15220", "#2a2f83"),
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
        opts_viz$map_tiles <- "CartoDB.Voyager"
        #opts_viz$palette_colors <- c("#7CDFEA", "#066C63")
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
      print(opts_viz$agg)
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
  
      if (r$active_viz %in% c("bubbles")) {
      lv <- lv %>% 
      leaflet::setView(lng = median(optsViz()$data$longitud, na.rm = TRUE), 
                       lat = median(optsViz()$data$latitud, na.rm = TRUE), zoom = 13)
      }
      lv
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
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                               lengthChange = F,
                               pageLength = 4,
                               scrollX = T,
                               scrollY = T,
                               initComplete = htmlwidgets::JS(
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
      leaflet::leafletOutput(ns("viz_lflt"), height = 490)  
      }else {
      highcharter::highchartOutput(ns("viz_hgch"), height = 490)
      },
      tags$a(href="https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj",
             "Fuente: Portal de datos abiertos de CDMX", target="_blank")
      )
      },
      error = function(cond) {
        return()
      }) 
    })
    
    
    output$summaryInfo <- renderUI({
      req(r$d_sum)
      print(r$d_sum)
      Fmv <- r$d_sum %>% dplyr::filter(Sexo == "Femenino")
      Mmv <- r$d_sum %>% dplyr::filter(Sexo == "Masculino")
      Nmv <- r$d_sum %>% dplyr::filter(is.na(Sexo))

      HTML(paste0(
        "<div class = 'dataSummary'>",
        "<div class = 'infoAll'>",format(sum(r$d_sum$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>Víctimas</span></div>",
        "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(sum(Fmv$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>Mujeres</span></div>",
        "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(sum(Mmv$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>Hombres</span></div>",
        "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(sum(Nmv$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>No identificados</span></div>
        </div>"
      ))

    })
    
    output$infoButt <- renderUI({
      div(style = "display: flex;gap:20px;margin-right: 0 !important;",
      actionButton(ns("descripcion_modal"), "Descripción"),
      actionButton(ns("dicc_modal"), "Diccionario"),
      actionButton(ns("recursos_modal"), "Recursos")
      )
    })
    
    observe({
      r$downViz <- viz_s()
      r$modal_desc <- input$descripcion_modal
      r$modal_dicc <- input$dicc_modal
      r$modal_recs <- input$recursos_modal
      r$dic_violencia <- dicViolencia
      })
    
  })
}

## To be copied in the UI
# mod_load_viz_ui("load_viz_ui_1")

## To be copied in the server
# mod_load_viz_server("load_viz_ui_1")
