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
      if (is.null(r$d_sel)) return()
      if (r$quest_choose != "violencia") return()
      setNames(c("Sexo", "Categoria", "competencia", "AlcaldiaHechos"),
               c("Sexo", "Categoria", "Competencia", "Alcaldias"))
    })
    
    plotSel <- reactive({
      req(r$active_viz)
      r$active_viz
    })
    
    varTwoSel <- reactive({
      req(r$varViewId)
      length(r$varViewId) == 2
    })
    
    
    catg_opts <- reactive({
      if (is.null(r$d_sel)) return()
      df <- r$d_sel
      c("TODAS",unique(df$Categoria))
    })

    jur_opts <- reactive({
      if (is.null(r$d_sel)) return()
      df <- r$d_sel
      c("TODAS",unique(df$CalidadJuridica))
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
    
    observe({
      r$parmesan_input <- parmesan_input()
    })
    
    
    
  })
}

## To be copied in the UI
# mod_load_parmesan_ui("load_parmesan_ui_1")

## To be copied in the server
# mod_load_parmesan_server("load_parmesan_ui_1")
