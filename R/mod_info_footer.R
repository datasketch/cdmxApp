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
    div(style = "display:flex;align-items: center;background: #F7F7F7;justify-content: space-between;",
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
      tryCatch({
        req(r$d_sum)
        print(r$d_sum)
        HTML(paste0(
          "<div class = 'dataSummary'>",
          "<div class = 'infoAll'>",format(nrow(r$d_sel), big.mark = ","), "<span class = 'infoAdd'>Total</span></div>",
          "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(nrow(r$d_sel) - r$d_sum$Total, big.mark = ","), "<span class = 'infoAdd'>Filtrado</span></div>",
          "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format((r$d_sum$Total/nrow(r$d_sel))*100, big.mark = ",", digits = 2, nsmall = 2), "<span class = 'infoAdd'>% del total</span></div>",
         # "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(sum(Nmv$Total, na.rm = TRUE), big.mark = ","), "<span class = 'infoAdd'>No identificados</span></div>
        "</div>"
        ))
      },
      error = function(cond) {
        return()
      })
    })
    
    output$infoButt <- renderUI({
      div(style = "display: flex;gap:20px; margin: 1px 20px 1px 0px;",
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
