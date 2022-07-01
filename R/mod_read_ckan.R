#' read_ckan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_ckan_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' read_ckan Server Functions
#'
#' @noRd 
mod_read_ckan_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    infoUrl <- reactive({
      tryCatch({
      generalUrl <- "https://datos-prueba.cdmx.gob.mx/api/3/action/resource_show?id="
      linkInfo <- r$url_par
      if (is.null(linkInfo)) linkInfo <- "ff1d4cbf-5985-45db-b40f-d820ce2b01a2"#"d543a7b1-f8cb-439f-8a5c-e56c5479eeb5"    # "140e35f9-9244-4b45-b638-816c2ab7651a"#"ff1d4cbf-5985-45db-b40f-d820ce2b01a2"#"d543a7b1-f8cb-439f-8a5c-e56c5479eeb5"###"2263bf74-c0ed-4e7c-bb9c-73f0624ac1a9" #"b089368e-f710-4f4b-9bae-f9f154d46220" 
      linkInfo <- paste0(generalUrl, linkInfo)
      listConf <- jsonlite::fromJSON(linkInfo)
      listConf$result
      },
      error = function(cond) {
        return()
      })
    })
    
    #con <- NULL
    
    dataCkan <- reactive({
     # tryCatch({
      req(infoUrl())
      #file <- listConf$result$url
      file <- infoUrl()$url
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      #csv <- readr::read_csv(file, na = c("NA", ""))
      csv <- readr::read_csv("sampleData.csv")
      DBI::dbWriteTable(con, "cdmxData", csv)
      
      con
      # },
      # error = function(cond) {
      #   return()
      # })
    })
    
    dicCkan <- reactive({
     # tryCatch({
      req(infoUrl())
      #idDic <- listConf$result$package_id
      idDic <- infoUrl()$package_id
      if (is.null(idDic)) return()
      #idDic <- "7593b324-6010-44f7-8132-cb8b2276c842"
      dicUrl <- paste0("https://datos-prueba.cdmx.gob.mx/api/3/action/package_show?id=", idDic)
      listDic <- jsonlite::fromJSON(dicUrl)
      listDic <- 
        purrr::map(seq_along(listDic), function(i) {
          if (!"resources" %in% names(listDic[[i]])) return()
          listDic[[i]]
        }) %>% 
        purrr::discard(is.null) %>% 
        .[[1]] 
   
      listUrl <- 
        listDic %>% 
        .$resources #%>%
      
      if ("datastore_active" %in% names(listUrl)) {
        listUrl <-  listUrl %>% dplyr::filter(!datastore_active) 
      }
      listUrl <- listUrl %>%   dplyr::select(name, format, url)
      listUrl$format <- gsub("\\.", "",tolower(listUrl$format))
      
      dataDic <- listUrl %>% dplyr::filter(format != "pdf")
      
      if (nrow(dataDic) == 0 ) {
        dataDic <- NULL  #stop("Debe ingresar el diccionario en formato xlsx o csv")
      } else {
        if (dataDic$format == "csv") {
          dataDic <- readr::read_csv(dataDic$url)
        } else {
          dataDic <- rio::import(dataDic$url)
        }
      }
      
      
      infoDic <- list(
        titulo = listDic$title,
        notas = listDic$notes
      )
      
      listDic <- list(
        listResources = listUrl,
        dataDic = dataDic,
        infoDic = infoDic
      )
      
      listDic
      # },
      # error = function(cond) {
      #   return()
      # })
    })
    
    
    observe({
      r$ckanConf <- infoUrl()
      r$ckanData <- dataCkan()
      r$ckanExtra <- dicCkan()
    })
    
  })
}

## To be copied in the UI
# mod_read_ckan_ui("read_ckan_1")

## To be copied in the server
# mod_read_ckan_server("read_ckan_1")
