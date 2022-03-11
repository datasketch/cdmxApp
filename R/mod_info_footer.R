#' info_footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_info_footer_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(style = "display:flex;align-items: center;justify-content: space-between;",
        uiOutput(ns("summaryInfo")),
        uiOutput(ns("infoButt"))
    )
  )
}
    
#' info_footer Server Functions
#'
#' @noRd 
mod_info_footer_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$summaryInfo <- renderUI({
      req(r$d_sum)
      #print(r$d_sum)
      Fmv <- r$d_sum %>% dplyr::filter(Sexo == "Femenino")
      Mmv <- r$d_sum %>% dplyr::filter(Sexo == "Masculino")
      Nmv <- r$d_sum %>% dplyr::filter(is.na(Sexo))
      
      HTML(paste0(
        "<div class = 'dataSummary'>",
        "<div class = 'infoAll'>",format(sum(r$d_sum$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>Víctimas</span></div>",
        "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(sum(Fmv$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>Mujeres</span></div>",
        "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(sum(Mmv$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>Hombres</span></div>",
        "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(sum(Nmv$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>No identificados</span></div>
        </div>"
      ))
      
    })
    
    output$infoButt <- renderUI({
      div(style = "display: flex;gap:20px;margin-right: 0 !important;",
          actionButton(ns("descripcion_modal"), "Descripción"),
          actionButton(ns("dicc_modal"), "Diccionario"),
          actionButton(ns("recursos_modal"), "Recursos")
      )
    })
    
    observe({
      r$modal_desc <- input$descripcion_modal
      r$modal_dicc <- input$dicc_modal
      r$modal_recs <- input$recursos_modal
      r$dic_violencia <- dicViolencia
    })
    
    
  })
  

}
    
## To be copied in the UI
# mod_info_footer_ui("info_footer_ui_1")
    
## To be copied in the server
# mod_info_footer_server("info_footer_ui_1")
