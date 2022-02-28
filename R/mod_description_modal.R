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
      req(dicViolencia)
      df <- dicViolencia[,-3]
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

    output$info_recs <- renderUI({
      "At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat."
    })
    
  })
}
    
## To be copied in the UI
# mod_description_modal_ui("description_modal_ui_1")
    
## To be copied in the server
# mod_description_modal_server("description_modal_ui_1")
