#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Define reactive value to pass parameters between modules
  r <- reactiveValues()
  
  # print buttons
  mod_questions_buttons_server("questions_buttons_ui_1", r)
  
  
  quest_choose <- reactive({
    last_btn <- input$last_click
    if (is.null(last_btn)) last_btn <- "violencia"
    last_btn
  })
  
  observe({
    r$quest_choose <- quest_choose()
  })
  
  mod_load_parmesan_server("load_parmesan_ui_1", r)
  mod_viz_selection_server("viz_selection_ui_1", r)
  mod_selected_data_server("selected_data_ui_1", r)
  mod_filter_data_server("filter_data_ui_1", r)
  mod_viz_data_server("viz_data_ui_1", r)
  #mod_data_to_table_server("data_to_table_ui_1", r)
  mod_viz_type_server("viz_type_ui_1", r)
  mod_load_viz_server("load_viz_ui_1", r)
  mod_click_info_server("click_info_ui_1", r)
  mod_download_viz_server("download_viz_ui_1", r)
  mod_description_modal_server("description_modal_ui_1", r)
}
