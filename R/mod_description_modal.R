#' description_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_description_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinypanels::modal(id = ns('modal_viz_info'), title = "Descripción", uiOutput(ns("info_plots"))),
    shinypanels::modal(id = ns('modal_dic_info'), title = "Diccionario", uiOutput(ns("infoDics"))),
    shinypanels::modal(id = ns('modal_rec_info'), title = "Recursos", uiOutput(ns("info_recs")))
  )
}
    
#' description_modal Server Functions
#'
#' @noRd 
mod_description_modal_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(r$modal_desc, {
      shinypanels::showModal(ns("modal_viz_info"))
    })
    
    output$info_plots <- renderUI({
      HTML(
      "
      <p>Esta base de datos contiene la información actualizada de las <span style='font-weight: 400;'>víctimas de
      los delitos en las carpetas de investigación de la Fiscalía General de 
      Justicia (FGJ) de la Ciudad de México a partir de enero de 2019. </span>
      <p/>
      <p>
      Para una correcta interpretación de la información, se realizan las siguientes 
      aclaraciones. 
      <p/>
      <p>
      El campo 'FechaHecho' representa la fecha en la que se cometió. 
      <p/>
      <p>
      El campo 'FechaInicio' corresponde a la fecha de la apertura de la carpeta de 
      investigación. 
      <p/>
      <p>
      En esta base se señala el sexo de la víctima, 
      así como la fecha en que ocurrieron los hechos denunciados.
      <p/>
      <p>
      Es importante destacar que, aunque en algunas ocasiones la víctima es la denunciante,
      en otras, denunciante y víctima son diferentes (por ejemplo, casos en los que menores 
      de edad son víctimas). Es posible que en una misma denuncia se incluya a una o más 
      víctimas.
      <p/>
      <p>
      Estas y otras aclaraciones las puedes encontrar en el Diccionario que se 
      adjunta a la presente base. Los datos de esta base fueron actualizados por la FGJ el 29 de
      julio de 2020. En esta página se puede descargar una nota realizada por la FGJ en la que
      se explica el proceso de la actualización y la nueva información. 
      Los datos anteriores a la podrán seguir consultándose.
      <p/>"
      )
    })
    
    observeEvent(r$modal_dicc, {
      shinypanels::showModal(ns("modal_dic_info"))
    })

    output$tableDic <- DT::renderDataTable({
      req(r$ckanExtra)
      df <- r$ckanExtra$dataDic
      dtable <- DT::datatable(df,
                              rownames = F,
                              escape = FALSE,
                              selection = 'none',
                              options = list(
                                lengthChange = F,
                                pageLength = nrow(df),
                                scrollX = T,
                                scrollY = T,
                                dom = 't')
                              ) %>% 
        DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
      
      
      dtable
    })
    

    output$infoDics <- renderUI({
      div(
      dsmodules::downloadTableUI(ns("dropdown_dic"), dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown"),
      DT::dataTableOutput(ns("tableDic"))
      )
    })
    
    
    observe({
      dsmodules::downloadTableServer("dropdown_dic", element = reactive(r$dic_violencia), formats = c("csv", "xlsx", "json"))
    })
    
    

    observeEvent(r$modal_recs, {
      shinypanels::showModal(ns("modal_rec_info"))
    })

    output$PdfReclasificacion <- downloadHandler(
      filename = "nota-reclasificacion.pdf",
      content = function(file) {
        file.copy(app_sys("app/www/recursos/nota-reclasificacion.pdf"), file)
      }
    )
    
    output$PdfUpdate <- downloadHandler(
      filename = "nota-actualizacion.pdf",
      content = function(file) {
        file.copy(app_sys("app/www/recursos/nota-actualizacion.pdf"), file)
      }
    )
    
    
    output$PdfOldUpdate <- downloadHandler(
      filename = "nota-actualizacion-290720.pdf",
      content = function(file) {
        file.copy(app_sys("app/www/recursos/nota-actualizacion-290720.pdf"), file)
      }
    )
    
    output$info_recs <- renderUI({
      div(style="display: inline-grid;gap: 21px;",
      downloadLink(ns("PdfReclasificacion"), "Nota Reclasificación"),
      downloadLink(ns("PdfUpdate"), "Nota para la Actualización"),
      downloadLink(ns("PdfOldUpdate"), "Nota Actualización 29/07/20")
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_description_modal_ui("description_modal_ui_1")
    
## To be copied in the server
# mod_description_modal_server("description_modal_ui_1")
