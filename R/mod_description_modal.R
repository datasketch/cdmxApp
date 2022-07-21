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
      tx <- r$ckanConf$name
      if (is.null(r$ckanConf$name)) tx <- ""
      ds <- r$ckanConf$description
      if (is.null(r$ckanConf$description)) ds <- ""
      HTML(paste0(
        "<h4>", tx, "</h4><br/>",
        "<p>", ds, "</p>"
      ))
    })
    
    observeEvent(r$modal_dicc, {
      shinypanels::showModal(ns("modal_dic_info"))
    })

    output$tableDic <- DT::renderDataTable({
      req(r$ckanExtra)
      df <- r$ckanExtra$dataDic
      if (is.null(df)) return()
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

    
    
    # output$PdfUpdate <- downloadHandler(
    #   filename = "nota-actualizacion.pdf",
    #   content = function(file) {
    #     download.file(url = "https://datos-prueba.cdmx.gob.mx/dataset/7593b324-6010-44f7-8132-cb8b2276c842/resource/8ec51d06-b430-4722-a811-2cd396c4ba66/download/nota-para-la-actualizacion-victimas-en-carpetas-de-investigacion-pgj.pdf",
    #                   destfile = paste0(app_sys("app/www/recursos/"), "/pdf1.pdf"))
    #     file.copy(app_sys("app/www/recursos/pdf1.pdf"), file)
    #   }
    # )
    observe({
      # infoResources <- listDic$listResources$format
      infoResources <- r$ckanExtra$listResources
      if (is.null(infoResources)) return()
      purrr::map(1:nrow(infoResources), function(i) {
        output[[paste0("infoResources", i)]] <- downloadHandler(
          filename = paste0(infoResources$name[i], ".", infoResources$format[i]),
          content = function(file) {
            saveFile <- paste0(tempdir(), "/pdf", i, ".", infoResources$format[i])
            print(saveFile)
            download.file(url = infoResources$url[i],
                          destfile = saveFile)
            file.copy(saveFile, file)
          }
        )
      })
    })
    
    
    
    output$info_recs <- renderUI({
      infoResources <- r$ckanExtra$listResources
      if (is.null(infoResources)) return() 
      
      div(style="display: inline-grid;gap: 21px;",
      purrr::map(1:nrow(infoResources), function(i) {
            downloadLink(ns(paste0("infoResources", i)), infoResources$name[i])
        
      })
      )
    })
    
    # output$PdfReclasificacion <- downloadHandler(
    #   filename = "nota-reclasificacion.pdf",
    #   content = function(file) {
    #     file.copy(app_sys("app/www/recursos/nota-reclasificacion.pdf"), file)
    #   }
    # )
    # 
    # 
    # 
    # 
    # output$PdfOldUpdate <- downloadHandler(
    #   filename = "nota-actualizacion-290720.pdf",
    #   content = function(file) {
    #     file.copy(app_sys("app/www/recursos/nota-actualizacion-290720.pdf"), file)
    #   }
    # )
    # 
    # output$info_recs <- renderUI({
    #   div(style="display: inline-grid;gap: 21px;",
    #   downloadLink(ns("PdfReclasificacion"), "Nota Reclasificación"),
    #   downloadLink(ns("PdfUpdate"), "Nota para la Actualización"),
    #   downloadLink(ns("PdfOldUpdate"), "Nota Actualización 29/07/20")
    #   )
    # })
  })
}
    
## To be copied in the UI
# mod_description_modal_ui("description_modal_ui_1")
    
## To be copied in the server
# mod_description_modal_server("description_modal_ui_1")
