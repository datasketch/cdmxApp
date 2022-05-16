#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinypanels::panelsPage(
      mod_description_modal_ui("description_modal_ui_1"),
      shinypanels::panel(title = " ",
                         id = "azul",
                         width = 330,
                         can_collapse = FALSE,
                         body = div(
                           mod_filter_index_ui("filter_index_ui_1"),
                           # div(style = "max-height: 300px !important; overflow: auto; margin-bottom: 5%;",
                           #     mod_questions_buttons_ui("questions_buttons_ui_1")
                           # ),
                           mod_load_parmesan_ui("load_parmesan_ui_1")
                         ),
                         
                         footer =
                           tags$a(
                             href="https://www.datasketch.co", target="blank",
                             img(src= 'www/img/ds_logo.svg', align = "left", width = 180, height = 150))
      ),
      shinypanels::panel(title = " ",
                         id = "naranja",
                         header_right = div(style = "display: flex;",
                                            div(class='first-container',
                                                mod_viz_selection_ui("viz_selection_ui_1")),
                                            div(class = 'inter-container', style = "margin-right: 3%;",
                                                # actionButton(
                                                #   "fs", "Full screen", 
                                                #   onclick = "gopenFullscreen();"
                                                # ) img(src= 'www/img/full.png')
                                                actionButton(inputId ='fs', "Fullscreen", onclick = "gopenFullscreen();")
                                                ),
                                            div(class='second-container',
                                                mod_download_viz_ui("download_viz_ui_1"))),
                         can_collapse = FALSE,
                         color = "chardonnay",
                         body = div(#shinybusy::add_busy_spinner(spin = "fading-circle"),
                           mod_load_viz_ui("load_viz_ui_1")
                         ),
                         footer = mod_info_footer_ui("info_footer_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  
  addResourcePath(
    'www', system.file('app/www', package = 'cdmxApp')
  )
  
  addResourcePath(
    'viz_icons', app_sys('app/viz_icons')
  )
  

  
  tags$head(
    favicon(),
    shinyjs::useShinyjs(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cdmxApp'
    ),
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    
    
    # include busy start up symbol
    shinybusy::busy_start_up(
      loader = tags$img(
        src = "www/img/loading_gris.gif",
        width = 100), 
      mode = "auto",
      color = "#435b69",
      background = "#FFF")
  )
}

