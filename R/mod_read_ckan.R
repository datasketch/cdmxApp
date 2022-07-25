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
        if (is.null(linkInfo)) linkInfo <- "d543a7b1-f8cb-439f-8a5c-e56c5479eeb5"#ede8e4df-02cb-459f-ab29-78a0610c99c8"#"d543a7b1-f8cb-439f-8a5c-e56c5479eeb5"#"47e49a86-733a-4bf6-93d1-4aa87d9ad60f" ## #"ff1d4cbf-5985-45db-b40f-d820ce2b01a2"#"140e35f9-9244-4b45-b638-816c2ab7651a"#"ff1d4cbf-5985-45db-b40f-d820ce2b01a2"#""###"2263bf74-c0ed-4e7c-bb9c-73f0624ac1a9" #"b089368e-f710-4f4b-9bae-f9f154d46220" 
        linkInfo <- paste0(generalUrl, linkInfo)
        listConf <- jsonlite::fromJSON(linkInfo)
        listConf$result
      },
      error = function(cond) {
        return()
      })
    })
    
    dicCkan <- reactive({
      # tryCatch({
      req(infoUrl())
      #idDic <- listConf$result$package_id
      idDic <- infoUrl()$package_id
      if (is.null(idDic)) return()
      dicUrl <- paste0("https://datos-prueba.cdmx.gob.mx/api/3/action/package_show?id=", idDic)
      listDic <- jsonlite::fromJSON(dicUrl)
      listDic <- 
        purrr::map(seq_along(listDic), function(i) {
          if (!"resources" %in% names(listDic[[i]])) return()
          listDic[[i]]
        }) %>% 
        purrr::discard(is.null) %>% 
        .[[1]] 
      
      dateFormat <- setdiff((listDic$resources$date_format%>% unlist()), c(NA, ""))
      if (identical(dateFormat, character())) dateFormat <- NULL
      if (idDic == "c9e96ab4-c127-4ee1-a222-bfa2b5d759de") dateFormat <- "a_m_d"
      if (idDic == "7593b324-6010-44f7-8132-cb8b2276c842") dateFormat <- "d_m_a"
      if (idDic == "12d22477-bcf1-49ee-92aa-16a0d0a5817c") dateFormat <- "d_m_a_hms"
      
      
      listDic <- 
        listDic %>% 
        .$resources #%>%
      
      
      listUrl <- listDic %>%   dplyr::select(name, format, url)
      listUrl$format <- gsub("\\.", "",tolower(listUrl$format))
      
      dataDic <- listUrl[grep("Dic", listUrl$name),]
      
      tryCatch({
        if (nrow(dataDic) == 0 ) {
          dataDic <- NULL  #stop("Debe ingresar el diccionario en formato xlsx o csv")
        } else {
          if (dataDic$format == "csv") {
            dataDic <- readr::read_csv(dataDic$url)
          } else {
            dataDic <- rio::import(dataDic$url)
          }
        }
      },
      error = function(cond) {
        dataDic <- NULL
      })
      
      infoDic <- list(
        titulo = listDic$title,
        notas = listDic$notes
      )
      
      listDic <- list(
        listResources = listUrl,
        dataDic = dataDic,
        infoDic = infoDic,
        dateFormat = dateFormat
      )
      
      listDic
      # },
      # error = function(cond) {
      #   return()
      # })
    })
    
    
    dataCkan <- reactive({
      # tryCatch({
      req(infoUrl())
      req(dicCkan())
      #file <- listConf$result$url
      file <- infoUrl()$url
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      
      ext <- substring(file, regexpr("\\.([[:alnum:]]+)$", file) + 1L)
      #csv <- readr::read_csv("sampleData.csv")
      if (ext == "csv") {
        encode <-
          tryCatch({
            readr::guess_encoding(file)$encoding[1]
          },
          error = function(cond) {
            return()
          })
        
        if (is.null(encode)) {
          csv <-
            tryCatch({
              readr::read_csv(file, na = c("NA", "", "nula"))
            },
            error = function(cond) {
              return()
            }) 
        } else {
        csv <-
          tryCatch({
            readr::read_csv(file, na = c("NA", "", "nula"), locale = readr::locale(encoding = encode), show_col_types = FALSE)
          },
          error = function(cond) {
            return()
          })
        }
      }
      if (ext == "xlsx") {
        csv <-
          tryCatch({
            rio::import(file)
          },
          error = function(cond) {
            return()
          }) 
      }
      
      if (!is.null(csv)) {
      csv <- Filter(function(x) !all(is.na(x)), csv)
      
      #listConf$result$date_format
      
      #if (!is.null(dicCkan()$dateFormat)) {
      indFecha <- grep("fecha|Fecha", names(csv))
      
      if (!identical(indFecha, integer())) {
        dateFormat <- dicCkan()$dateFormat
        if (is.null(dateFormat)) dateFormat <- "d_m_a"
        lf <- 
          purrr::map(indFecha, function (i) {
            if (dateFormat %in% c("d_m_a", "d/m/a")) {
              csv[[i]] <<- as.character(lubridate::dmy(csv[[i]])) }
            if (dateFormat %in% c("a_m_d", "a/m/d")) {
              csv[[i]] <<- as.character(lubridate::ymd(csv[[i]])) }
            if (dateFormat %in% c("m_a_d", "m/a/d")) {
              csv[[i]] <<- as.character(lubridate::myd(csv[[i]])) }
            if (dateFormat %in% c("d_m_a_hms")) {
              csv[[i]] <<- as.character(lubridate::as_date(lubridate::ymd_hms(csv[[i]]))) }
            csv[[paste0("temporal_", names(csv)[i])]] <<- format(as.Date(csv[[i]]), "%Y-%m")
          })
      }
      #}
      
      DBI::dbWriteTable(con, "cdmxData", csv, extended_types = T)
      
      con
      }
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
