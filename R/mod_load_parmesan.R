#' load_parmesan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr %>% 
mod_load_parmesan_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("controls"))
  )
}

#' load_parmesan Server Functions
#'
#' @noRd 
mod_load_parmesan_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dataId <- reactive({
      r$quest_choose
    })
    
    var_opts <- reactive({
      req(r$active_viz)
      if (is.null(r$d_sel)) return()
      if (r$quest_choose != "violencia") return()
      if (!(r$active_viz %in% c("bar", "treemap"))) return() 
      setNames(c("AlcaldiaHechos", "Sexo", "Categoria", "competencia"),
               c("Alcaldias", "Sexo", "Categoria", "Competencia"))
    })
    
    varExtra_opts <- reactive({
      req(r$active_viz)
      if (is.null(r$d_sel)) return()
      if (r$quest_choose != "violencia") return()
      if (r$active_viz %in% c("bar", "treemap")) return() 
      if (r$active_viz %in% c("map")) {
        ch <-  setNames(c("AlcaldiaHechos"),
                        c("Alcaldias"))
      } else {
        ch <- setNames(c("AlcaldiaHechos", "Sexo", "Categoria", "competencia"),
                       c("Alcaldias", "Sexo", "Categoria", "Competencia"))
      }
      ch
    })
    
    alcaldias_opts <- reactive({
      if (is.null(r$d_sel)) return()
      if (is.null(r$active_viz)) return()
      if (r$quest_choose != "violencia") return()
      if (r$active_viz == "map") {
        req(r$mapType)
      if (r$mapType  %in% c("bubbles", "heatmap")) {
        ddL <- alcaldiasCdmx %>% dplyr::filter(idAlcaldias == "CDMX ALCALDÍAS")
        ch <-  c("CDMX",unique(ddL$AlcaldiaHechos))
      } else if (r$mapType == "choropleth") {
        ch <- "CDMX"
      }} else {
        ch <- c("CDMX", "TODAS", unique(alcaldiasCdmx$AlcaldiaHechos))
      }
      ch
    })
    
    alcaldias_sel <- reactive({
      req(alcaldias_opts())
      alcaldias_opts()[1]
    })
    
    plotSel <- reactive({
      req(r$active_viz)
      r$active_viz
    })
    
    varTwoSel <- reactive({
      req(r$varViewId)
      length(r$varViewId) #== 2
    })
    
    stackLabel <- reactive({
      HTML("<span style='margin-left:5px; margin-top: -6px;'>Apilar barras </span>")
    })
    
    axisLabel <- reactive({
      HTML("<span style='margin-left:5px; margin-top: -6px;'> Cambiar de orden las variables </span>")
    })
    
    
    colors_default <- reactive({
      list(
        palette_a = c("#7CDFEA", "#50D0BE", "#35BDA4", "#22A990", "#14947F", "#0A8070", "#066C63"),
        palette_b = c("#EED7BA", "#EDC19F", "#EAAC85", "#E6966C", "#E08053", "#D96A3A", "#D15220"),
        palette_c = c("#066c63", "#39d361", "#f9e928", "#0a87cc", "#eed7ba", "#d15220", "#2a2f83")
      )
    })
    
    colors_show <- reactive({
      if (is.null(colors_default())) return()
      cd <- colors_default()
     lc <- purrr::map(names(cd), function(palette) {
       # palette <- "palette_a"
        colors <- cd[[palette]]
       as.character( div(
        purrr::map(colors, function(color) {
          div(style=paste0("width: 20px; height: 20px; display: inline-block; background-color:", color, ";"))
        })
        ))
      }) 
     names(lc) <- names(cd)
     lc
    })
    
    agg_palette <- reactive({
      if (is.null(r$active_viz)) return()
      if (is.null(colors_show())) return()
      colors_show()
    })
    
    catg_opts <- reactive({
      if (is.null(r$d_sel)) return()
      df <- r$d_sel
      c("TODOS",unique(df$Categoria))
    })
    
    jur_opts <- reactive({
      if (is.null(r$d_sel)) return()
      df <- r$d_sel
      c("TODAS",unique(df$CalidadJuridica))
    })
    
    
    fec_opts <- reactive({
      setNames(c("Año_hecho", "Año_inicio"),
               c("Año en que se cometió el delito",
                 "Año en que se hizo la denuncia"))
    })
    
    fec_select <- reactive({
      "Año_hecho"
    })
    
    
    # Initialize parmesan
    path <- app_sys("app/app_config/parmesan")
    parmesan <- parmesan::parmesan_load(path)
    parmesan_input <- parmesan::parmesan_watch(input, parmesan)
    
    parmesan::output_parmesan("controls",
                              parmesan = parmesan,
                              #r = r,
                              input = input,
                              output = output,
                              session = session,
                              env = environment())
    # # ======================================================================================================================
    # Pass all inputs from parmesan to other parts of the app as reactiveValues
    parmesan_inputs <- purrr::map(parmesan, function(.x) { purrr::map_chr(.x$inputs, "id")}) %>% unlist(use.names = FALSE)
    
    observe({
      for(parmesan_input in parmesan_inputs){
        get_input <- input[[parmesan_input]]
        if(!is.null(get_input)){
          r[[parmesan_input]] <- get_input
        }
      }
    })
    
    
    li <- reactive({
      parmesan:::index_inputs(session = session, input = input, parmesan = parmesan) %>% plyr::compact()
    })
    
    id_parmesan <- reactive({
      req(parmesan)
      parmesan::parmesan_input_ids(parmesan = parmesan)
    })
    
    
    observe({
      r$parmesan_input <- parmesan_input()
      r$info_inputs <- li()
      r$info_ids <- id_parmesan()
      r$info_parmesan <- parmesan
      r$colorsPlot <- colors_default()
    })
    
    
    
  })
}

## To be copied in the UI
# mod_load_parmesan_ui("load_parmesan_ui_1")

## To be copied in the server
# mod_load_parmesan_server("load_parmesan_ui_1")
