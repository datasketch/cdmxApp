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
      generalUrl <- "https://datos-prueba.cdmx.gob.mx/api/3/action/resource_show?id="
      linkInfo <- r$url_par
      if (is.null(linkInfo)) linkInfo <- "d543a7b1-f8cb-439f-8a5c-e56c5479eeb5"  
      linkInfo <- paste0(generalUrl, linkInfo)
      listConf <- jsonlite::fromJSON(linkInfo)
      listConf$result
    })
    
    #con <- NULL
    
    dataCkan <- reactive({
      req(infoUrl())
      #file <- listConf$result$url
      file <- infoUrl()$url
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      
      # dataCkan <- readr::read_csv(file)
      # usethis::use_data(dataCkan, overwrite = TRUE)
      DBI::dbWriteTable(con, "cdmxData", readr::read_csv(file))
      con
      #d <- dplyr::tbl(con,  "cdmxData")
      #d
    })
    
    dicCkan <- reactive({
      req(infoUrl())
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
        .$resources %>% 
        dplyr::filter(is.na(resource_disaggregate)) %>% 
        dplyr::select(name, format, url)
      listUrl$format <- gsub("\\.", "",tolower(listUrl$format))
      
      dataDic <- listUrl %>% dplyr::filter(format != "pdf")
      if (nrow(dataDic) == 0) stop("Debe ingresar el diccionario en formato xlsx o csv")
      if (dataDic$format == "csv") {
        dataDic <- readr::read_csv(dataDic$url)
      } else {
        dataDic <- rio::import(dataDic$url)
      }
      
      
      infoDic <- list(
        titulo = listDic$title,
        notas = listDic$notes
      )
      
      listDic <- list(
        listResources = listUrl,
        dataDic <- dataDic,
        infoDic = infoDic
      )
      listDic
      
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
