#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Define reactive value to pass parameters between modules
  r <- reactiveValues()
  
  # Get parameters from URL
  par <- list(ckanConf = NULL)
  
  url_par <- reactive({
    shinyinvoer::url_params(par, session)
  })

  
  observe({
    r$url_par <- url_par()$inputs$ckanConf
  })
  
  mod_load_parmesan_server("load_parmesan_ui_1", r)
  
   
  observe({
    req(r$info_parmesan)
    if (identical(list(), r$info_parmesan)) return()
    req(r$info_ids)
    if (identical(list(), r$info_ids)) return()

    
    parmesanInputs <- r$info_parmesan
    parmesan::indexButtonsServer(session = session, input = input,
                                 id = "INDEXTEST", parmesan_ids = r$info_ids,
                                 parmesan_load = parmesanInputs, module_id = "load_parmesan_ui_1-")

  })
  
  

  
  mod_read_ckan_server("read_ckan_ui_1", r)
  mod_viz_selection_server("viz_selection_ui_1", r)
  mod_selected_data_server("selected_data_ui_1", r)
  mod_subsetting_data_server("subsetting_data_ui_1", r)
  mod_filter_data_server("filter_data_ui_1", r)
  mod_viz_data_server("viz_data_ui_1", r)
  #mod_data_to_table_server("data_to_table_ui_1", r)
  mod_viz_type_server("viz_type_ui_1", r)
  mod_load_viz_server("load_viz_ui_1", r)
  mod_download_viz_server("download_viz_ui_1", r)
  mod_info_footer_server("info_footer_ui_1", r)
  mod_description_modal_server("description_modal_ui_1", r)
  mod_filter_index_server("filter_index_ui_1", r)
}
