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
      req(data_select())
      df <- data_select()
      #dic <- r$ckanExtra
      lsFringe <- 
        df %>% 
        head(30) %>% 
        dplyr::collect() %>% 
        homodatum::fringe()
      lsFringe$dic$hdType[grepl("geo|colonia", lsFringe$dic$id)] <- "Gnm"
      lsFringe$dic$hdType[grepl("longitud", lsFringe$dic$id)] <- "Gln"
      lsFringe$dic$hdType[grepl("latitud", lsFringe$dic$id)] <- "Glt"
      lsFringe$dic$hdType[grepl("edad", lsFringe$dic$id)] <- "Cat"
      lsFringe$dic$hdType[grepl("hora|postal|id", lsFringe$dic$id)] <- "Txt"
      lsFringe$dic$hdType[grepl("fecha|date", lsFringe$dic$id)] <- "Dat"
      #print(lsFringe$dic)
      lsFringe
    })
    
    dic_fringe <- reactive({
      req(data_fringe())
      data_fringe()$dic
    })
    
    
    varsToFilter <- reactive({
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
      if (nrow(numDic) > 0) {
        numDic$id
      } else {
        return()
      }
      
    })
    
    numRange <- reactive({
      req(numToFilter())
        lNum <- 
          purrr::map(numToFilter(), function(var){
            minNum <- as.vector(DBI::dbGetQuery(r$ckanData, paste0("SELECT MIN(",var,") FROM cdmxData")))
            maxNum <- as.vector(DBI::dbGetQuery(r$ckanData, paste0("SELECT MAX(",var,") FROM cdmxData")))
            df <- data.frame(min = as.numeric(minNum), max = as.numeric(maxNum), id = var)
            df
          }) %>% dplyr::bind_rows()
        lNum
    })
    
    
    observe({
      r$d_sel <- data_select()
      r$dic_f <- dic_fringe()
      r$vars_f <- varsToFilter()
      r$allCats <- catsToFilter()
      r$allNums <- numToFilter()
      r$numRange <- numRange()
    })
    
  })
}

## To be copied in the UI
# mod_selected_data_ui("selected_data_ui_1")

## To be copied in the server
# mod_selected_data_server("selected_data_ui_1")
