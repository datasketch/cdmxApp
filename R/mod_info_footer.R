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
        
        nrowIni <- as.vector(DBI::dbGetQuery(r$ckanData, "SELECT COUNT(*) FROM cdmxData"))$`COUNT(*)`
        print(nrowIni)
        
        pctgView <- (r$d_sum$Total/nrowIni)*100
        nDig <- 2
        if (pctgView == 100) nDig <- 0
        HTML(paste0(
          "<div class = 'dataSummary'>",
          "<div class = 'infoAll'>",format(nrowIni, big.mark = ","), "<span class = 'infoAdd'>Total</span></div>",
          "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format( r$d_sum$Total, big.mark = ","), "<span class = 'infoAdd'>Visualizados</span></div>",
          "<div class = 'infoAll' style = 'border-left: 1px solid;margin-left:3%;padding: 0% 3%;'>",format(pctgView, big.mark = ",", nsmall = nDig), "%<span class = 'infoAdd'> del total</span></div>",
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
      #r$dic_violencia <- dicViolencia
    })
    
    
  })
  
  
}

## To be copied in the UI
# mod_info_footer_ui("info_footer_ui_1")

## To be copied in the server
# mod_info_footer_server("info_footer_ui_1")
