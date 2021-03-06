#' selected_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selected_data_ui <- function(id){
   ns <- NS(id)
   tagList(
      
   )
}

#' selected_data Server Functions
#'
#' @noRd 
mod_selected_data_server <- function(id, r){
   moduleServer( id, function(input, output, session){
      ns <- session$ns
      
      data_select <- reactive({
         tryCatch({
            qs <- r$quest_choose
            df <- NULL
            if (qs == "violencia") {
               df <- dataVictimas
            } 
            df
         },
         error = function(cond) {
            return()
         })
      })
      
      
      varsToFilter <- reactive({
         data.frame(
            id = c("alcaldiasId", "calidadId", "categoriaId", "sexoId"),
            vars = c("AlcaldiaHechos", "CalidadJuridica", "Categoria", "Sexo")
         )
      })
      
      
      catsToFilter <- reactive({
         tryCatch({
            req(data_select())
            req(varsToFilter())
            df <- data_select()
            lCats <- 
               purrr::map(varsToFilter()$vars, function(var){
                  x <- c("Todas", as.character(unique(df[[var]])))
                  x[is.na(x)] <- "NA"
                  x
               })
            names(lCats) <- varsToFilter()$vars
            lCats
         },
         error = function(cond) {
            return()
         })
      })
      
      observe({
         r$d_sel <- data_select()
         r$vars_f <- varsToFilter()
         r$allCats <- catsToFilter()
      })
      
   })
}

## To be copied in the UI
# mod_selected_data_ui("selected_data_ui_1")

## To be copied in the server
# mod_selected_data_server("selected_data_ui_1")
