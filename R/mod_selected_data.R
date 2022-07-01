#' selected_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selected_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' selected_data Server Functions
#'
#' @noRd 
mod_selected_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_select <- reactive({
      tryCatch({
        req(r$ckanData)
        con <- r$ckanData
        df <- dplyr::tbl(con,  "cdmxData")
        df
      },
      error = function(cond) {
        return()
      })
    })
    
    data_fringe <- reactive({
      tryCatch({
      req(data_select())
      df <- data_select() %>% 
        head(30) %>% 
        dplyr::collect()
      lsFringe <- 
        df %>% 
        homodatum::fringe()
      lsFringe$dic$hdType[grepl("geo|colonia", lsFringe$dic$id)] <- "Gnm"
      lsFringe$dic$hdType[grepl("longitud", lsFringe$dic$id)] <- "Gln"
      lsFringe$dic$hdType[grepl("latitud", lsFringe$dic$id)] <- "Glt"
      lsFringe$dic$hdType[grepl("edad", lsFringe$dic$id)] <- "Cat"
      lsFringe$dic$hdType[grepl("hora|postal|id", lsFringe$dic$id)] <- "Txt"
      lsFringe$dic$hdType[grepl("fecha|date", lsFringe$dic$id)] <- "Dat"
      lsFringe$dic$hdType[grepl("mes|Mes", lsFringe$dic$id)] <- "Mon"
      lsFringe$dic$hdType[grepl("_1", lsFringe$dic$id)] <- "Uid"
      lsFringe$dic$id <- lsFringe$dic$label
      #dic <- r$ckanExtra
      #print(lsFringe$dic)
      lsFringe
      },
      error = function(cond) {
        return()
      })
    })
    
    dic_fringe <- reactive({
      req(data_fringe())
      data_fringe()$dic
    })
    
    
    varsToFilter <- reactive({
      tryCatch({
      req(r$ckanConf)
      #vars <- listConf$result$resource_disaggregate
      vars <-  r$ckanConf$resource_disaggregate

      if (is.null(vars)) {
        req(data_fringe())
        dic <- data_fringe()$dic
        catVars <- dic %>% dplyr::filter(hdType == "Cat") %>% .$id
        vars <- 
          purrr::map(catVars, function(var){
            vars <- NULL
            uCats <- DBI::dbGetQuery(r$ckanData, paste0("SELECT DISTINCT(",var,") FROM cdmxData"))
            if (length(uCats[[var]]) <= 30) {
              vars <- var
            } 
            vars
          }) %>%  purrr::discard(is.null) %>% unlist() 
      } else {
      vars <- vars %>% 
        stringr::str_split(pattern = ",") %>% 
        .[[1]] %>% 
        trimws() %>% 
        stringi::stri_trans_general(id = "Latin-ASCII")
      }
      df <-  data.frame(
        id = paste0(tolower(vars), "Id"),
        vars = vars
      )
      df
      },
      error = function(cond) {
        return()
      })
    })
    
    
    catsToFilter <- reactive({
      tryCatch({
        req(r$ckanData)
        req(varsToFilter())
        lCats <- 
          purrr::map(varsToFilter()$vars, function(var){
            uCats <- DBI::dbGetQuery(r$ckanData, paste0("SELECT DISTINCT(",var,") FROM cdmxData"))
            x <- c("Todas", as.character(uCats[[var]]))
            #x[is.na(x)] <- "NA"
            x
          })
        names(lCats) <- varsToFilter()$vars
        lCats
      },
      error = function(cond) {
        return()
      })
    })
    
    
    numToFilter <- reactive({
      req(data_fringe())
      dic <- data_fringe()$dic
      numDic <- dic %>% dplyr::filter(hdType == "Num")
      varNum <- NULL
      if (nrow(numDic) == 0) return()
      if (nrow(numDic) > 0) varNum <-  numDic$id
      varNum 
    })
    
    numRange <- reactive({
      if (is.null(numToFilter())) return()
      tryCatch({
        lNum <- 
          purrr::map(numToFilter(), function(var){
            minNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MIN(",var,") FROM cdmxData"))
            maxNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MAX(",var,") FROM cdmxData"))
            df <- data.frame(min = as.numeric(minNum[[1]]), max = as.numeric(maxNum[[1]]), id = var)
            df
          }) %>% dplyr::bind_rows()
        lNum
      },
      error = function(cond) {
        return()
      })
    })
    
    
    
    dateToFilter <- reactive({
      req(data_fringe())
      dic <- data_fringe()$dic
      print(dic)
      dateDic <- dic %>% dplyr::filter(hdType == "Dat")
      if (nrow(dateDic) > 0) {
        dateDic$id
      } else {
        return()
      }
    })
    
    dateRange <- reactive({
      tryCatch({
        req(dateToFilter())
        lNum <- 
          purrr::map(dateToFilter(), function(var){
            minNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MIN(",var,") FROM cdmxData"))
            maxNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MAX(",var,") FROM cdmxData"))
            df <- data.frame(min = lubridate::dmy(minNum[[1]]), max = lubridate::dmy(maxNum[[1]]), id = var)
            df
          }) %>% dplyr::bind_rows()
        lNum
      },
      error = function(cond) {
        return()
      })      
    })
    
    
    
    observe({
      r$d_sel <- data_select()
      r$dic_f <- dic_fringe()
      r$vars_f <- varsToFilter()
      r$allCats <- catsToFilter()
      r$allNums <- numToFilter()
      r$numRange <- numRange()
      r$allDates <- dateToFilter()
      r$datesRange <- dateRange()
    })
    
  })
}

## To be copied in the UI
# mod_selected_data_ui("selected_data_ui_1")

## To be copied in the server
# mod_selected_data_server("selected_data_ui_1")
