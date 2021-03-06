#' filter_index UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_index_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("indexTest"))
  )
}

#' filter_index Server Functions
#'
#' @noRd 
mod_filter_index_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    
    output$indexTest <- renderUI({
      req(r$info_inputs)
      path_dic <- cdmxApp:::app_sys("app/app_config/")
      dic_yaml <- yaml::read_yaml(paste0(path_dic, "/", list.files(pattern = ".yaml", path = path_dic)))
      parmesan::indexButtonsUI(id = "INDEXTEST", label = "Filtros aplicados", update_all = TRUE,update_label = "Eliminar todos los filtros",
                               list_inputs = r$info_inputs, dic_yaml = dic_yaml, img_icon =  'www/img/close.svg')
    })
    
    
  
    
    # # observe({
    # #   if (is.null(aver())) return()
    # #   
    # #   purrr::map(aver(), function(btn) {
    # #     print(btn)
    #     observeEvent(input[["INDEXTEST-index-alcaldiasId"]], {
    #       print("hola")
    #     })
    # #   })
    # # })
   
   
  })
}

## To be copied in the UI
# mod_filter_index_ui("filter_index_ui_1")

## To be copied in the server
# mod_filter_index_server("filter_index_ui_1")
