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
    
  
    # subsett <- reactive({
    #   if (is.null(r$d_sel)) return()
    #   
    #   df <- r$d_sel
    #   
    #   if (!(all(is.null(r$alcaldiasId)))) {
    #     df <- df %>% dplyr::filter(AlcaldiaHechos %in% r$alcaldiasId)
    #   }
    #   #print("sexo")
    #   #print(r$sexoId)
    #   #print(setdiff(r$sexoId, unique(df$Sexo)))
    #   if (!(all(is.null(r$sexoId)))) {
    #     # if (anyNA(r$sexoId)) {
    #     #   
    #     # }
    #     print("fefefe")
    #     print(r$sexoId)
    #     df <- df %>% dplyr::filter(Sexo %in% gsub("1", "",r$sexoId))
    #   }
    #   
    #   isolate(df)
    # })
    # 
    # 
    # sexoSubSett <- reactive({
    #   req(r$d_sel)
    #   req(subsett())
    #   df_o <- data.frame(Sexo = setdiff(unique(r$d_sel$Sexo), NA))
    #   df <- subsett() %>% 
    #          dplyr::group_by(Sexo) %>%
    #            dplyr::summarise(total = dplyr::n()) %>%
    #              dplyr::mutate(label = paste0(Sexo, " (", total, ")"))
    #   df <- df_o %>% dplyr::left_join(df)
    #   df$id <- df$Sexo
    #   if (!identical(which(is.na(df$label)), integer())) {
    #     indNa <- which(is.na(df$label))
    #     print(indNa)
    #     for (i in 1:length(indNa)) {
    #       df$id[indNa[i]] <- paste0(df$Sexo[indNa[i]], i)
    #       df$label[indNa[i]] <- paste0(df$Sexo[indNa[i]], " (0)")
    #     }
    #   }
    #   # df$id <- ifelse(is.na(df$label), paste0(df$Sexo, "1"), df$id)
    #   # df$label <- ifelse(is.na(df$label), paste0(df$Sexo, " (0)"), df$label)
    #   
    #   print("hola")
    #   print(df)
    #   setNames(df$id, df$label)
    # })
    # 
    #   observe({
    #    r$dataSubSett <- subsett()
    #    r$dataSexo <- sexoSubSett()
    # })
    
  })
}

## To be copied in the UI
# mod_subsetting_data_ui("subsetting_data_ui_1")

## To be copied in the server
# mod_subsetting_data_server("subsetting_data_ui_1")
