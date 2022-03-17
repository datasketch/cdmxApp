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
      if (r$active_viz %in% c("map")) {
        ch <-  setNames(c("AlcaldiaHechos"),
                        c("Alcaldias"))
      } else {
        ch <- setNames(c("AlcaldiaHechos", "Sexo", "Categoria", "competencia"),
                       c("Alcaldias", "Sexo", "Categoria", "Competencia"))
      }
      ch
    })
    
    desVarOpts <- reactive({
      req(r$varViewId)
      
      varPsel <- data.frame(id = c("ninguna", "AlcaldiaHechos", "Sexo", "Categoria", "competencia"),
                            label = c("Ninguna", "Alcaldias", "Sexo", "Categoria", "Competencia"))
      varPsel <- varPsel %>% dplyr::filter(id != r$varViewId)
      
      setNames(varPsel$id, varPsel$label)
      
      
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
      req(r$desagregacionId)
      r$desagregacionId != "ninguna"
    })
    
    stackLabel <- reactive({
      HTML("<span style='margin-left:5px; margin-top: -6px;'>Apilar barras </span>")
    })
    
    axisLabel <- reactive({
      HTML("<span style='margin-left:5px; margin-top: -6px;'> Cambiar de orden las variables </span>")
    })
    
    
    colors_default <- reactive({
      req(r$active_viz)
      if (r$active_viz != "bar") {
      list(
        palette_a = c("#B48E5D", "#C3A57D", "#CBB18E", "#D3BDA0", "#DCCAB2", "#E4D6C5", "#EDE3D7", "#F6F1EB"),
        palette_b = c("#1B5C51", "#4E786F", "#66887F", "#7E9992", "#96ACA5", "#AFBFBB", "#C8D4D1", "#E2EBE9"),
        palette_c = c("#0E709E", "#568BB2", "#709ABC", "#88A9C7", "#9FBAD2", "#B5CADD", "#CBDBE8", "#E0EDF3"),
        palette_d = c("#253786", "#52599C", "#696DA9", "#8182B6", "#999AC4", "#B1B1D2", "#CACADE", "#E1E2EB"),
        palette_e = c("#9E2348", "#B15267", "#BB6979", "#C6818D", "#D19AA3", "#DCB3B9", "#E8CCD1", "#F4E5E9"),
        palette_f = c("#B33718", "#C45633", "#CC6644", "#D47657", "#DD876B", "#E69880", "#EFAA96", "#F8BBAD")
        
      )
      } else {
        req(r$desagregacionId)
        if (r$desagregacionId != "ninguna") {
        list(
          palette_a = c("#3E9FCC", "#8A6BAC", "#EA5254", "#F18951", "#FCC448", "#71B365"),
          palette_b = c("#93D0F1", "#D8CEE4", "#EB9594", "#F9BE9B", "#FFE095", "#CBE3C6"),
          palette_c = c("#19719F", "#5D3A84", "#D02622", "#D16020", "#CF981B", "#438536")
        )
        } else {
        list(
            palette_a = c("#3E9FCC"),
            palette_b = c("#93D0F1"),
            palette_c = c("#19719F")
          )
        }
      } 
      
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
