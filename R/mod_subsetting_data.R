#' subsetting_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_subsetting_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' subsetting_data Server Functions
#'
#' @noRd 
mod_subsetting_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    labelVal <- reactiveValues(change = NULL)
    
    observe({
      req(r$d_fil)
      req(r$allCats)
      req(r$vars_f)
      df <- r$d_fil
      varsF <- r$vars_f
      l_lb <- 
        purrr::map(1:nrow(varsF), function(i) {
          df_o <- data.frame(id = r$allCats[[varsF$vars[i]]])
          df_o$labelAdd <- paste0(df_o$id, " (0)")
          df_s <- df %>% 
            dplyr::group_by_(id = varsF$vars[i]) %>% 
            dplyr::summarise(total = dplyr::n()) %>%
            tidyr::drop_na() %>%
            dplyr::mutate(label = paste0(id, " (", total, ")"))
          df_s <- dplyr::bind_rows(
                   data.frame(id = "Todas", label = paste0("Todas (", sum(df_s$total, na.rm = T), ")")),
                   df_s)
          if (nrow(df_s) > 0  | !is.null(df_s)) {
            df_o <- df_o %>% dplyr::left_join(df_s)
            df_o$label <- dplyr::coalesce(df_o$label, df_o$labelAdd)
          }
        }) %>% plyr::compact()
      
      if (identical(l_lb, list())) {
        labelVal$change <- NULL
      } else {
        names(l_lb) <- varsF$vars
        labelVal$change <- l_lb
      }
      labelVal$change <- l_lb
      r$labelChange <- labelVal$change
    })
    

    
  })
}

## To be copied in the UI
# mod_subsetting_data_ui("subsetting_data_ui_1")

## To be copied in the server
# mod_subsetting_data_server("subsetting_data_ui_1")
