#' questions_buttons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_questions_buttons_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("general_filter"))
  )
}

#' questions_buttons Server Functions
#'
#' @noRd 
mod_questions_buttons_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$general_filter <- renderUI({
      question_buttons(c("violencia"),#, "otra1", "otra2"), 
                       c("Víctimas en carpetas de investigación FGJ"#,
                         # "Otros datos 1",
                         # "Otros datos 2"
                         )
      )
    })
 
  })
}

## To be copied in the UI
# mod_questions_buttons_ui("questions_buttons_ui_1")

## To be copied in the server
# mod_questions_buttons_server("questions_buttons_ui_1")
