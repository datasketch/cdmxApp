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
        #print(r$ckanExtra)
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
        # df <- df %>%head(30) %>%dplyr::collect()
        df <- data_select() %>% 
          head(30) %>% 
          dplyr::collect()
        # lsFringe <- NULL
        lsFringe <-  df %>% 
          homodatum::fringe()
        #lsFringe$dic$hdType[grepl("fecha|date", tolower(lsFringe$dic$id))] <- "Dat"
        lsFringe$dic$hdType[grepl("mes|Mes", tolower(lsFringe$dic$id))] <- "Mon"
        lsFringe$dic$hdType[grepl("longitud|lon|long", tolower(lsFringe$dic$id))] <- "Gln"
        lsFringe$dic$hdType[grepl("latitud|lat", tolower(lsFringe$dic$id))] <- "Glt"
        lsFringe$dic$hdType[grepl("edad", tolower(lsFringe$dic$id))] <- "Cat"
        lsFringe$dic$hdType[grepl("hora|postal|id", tolower(lsFringe$dic$id))] <- "Txt"
        lsFringe$dic$hdType[grepl("_1", tolower(lsFringe$dic$id))] <- "Uid"
        lsFringe$dic$hdType[grepl("geo|colonia", tolower(lsFringe$dic$id))] <- "Gnm"
        lsFringe$dic$hdType[grepl("fecha_trim|ubicacion_web|cob_geo_ent_clave|cob_geo_ent|indicador_clave|geopoint|calle|ubicacion", tolower(lsFringe$dic$id))] <- "___"       
        lsFringe$dic$id <- lsFringe$dic$label
        lsFringe
        # },
        # error = function(cond) {
        #   list(dic = data.frame(id = names(df), label = names(df), hdType = "Cat"))
        # })
        
        
        #dic <- r$ckanExtra
        print(lsFringe$dic)
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
        #vars <- setdiff(listConf$result$resource_disaggregate, c(NA, ""))
        vars <-  setdiff(r$ckanConf$resource_disaggregate, c(NA, ""))
        
        if (identical(vars, character())) vars <- NULL
        if (is.null(vars)) {
          req(data_fringe())
          #dic <-  lsFringe$dic
          dic <- data_fringe()$dic
          catVars <- dic %>% dplyr::filter(hdType == "Cat") %>% .$id
          
          vars <- 
            purrr::map(catVars, function(var){
              vars <- NULL
              #uCats <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT(",var,") FROM cdmxData"))
              uCats <- DBI::dbGetQuery(r$ckanData, paste0("SELECT DISTINCT(",var,") FROM cdmxData"))
              
              if (length(uCats[[var]]) > 1 & length(uCats[[var]]) <= 30 ) {
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
          vars <- gsub("Competencia", "competencia", vars)
        }
        df <-  data.frame(
          id = paste0(tolower(vars), "Id"),
          vars = vars
        )
        print(df)
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
            #print(as.character(uCats[[var]]))
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
            minNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MIN(CAST(",var," AS INT)) FROM cdmxData"))
            maxNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MAX(CAST(", var," AS INT)) FROM cdmxData"))
            df <- data.frame(min = as.numeric(minNum[[1]]), max = as.numeric(maxNum[[1]]), id = var)
            if (!(df$min == df$max)) {
              df <- df %>% dplyr::filter(!is.na(min), !is.na(max))
              if (nrow(df) == 0) df <- NULL
            } else {
              df <- NULL
            }
            df
          }) %>% dplyr::bind_rows()
        lNum
      },
      error = function(cond) {
        return()
      })
    })
    
    
    
    dateToFilter <- reactive({
      tryCatch({
        if (is.null(r$ckanExtra$dateFormat)) return()
        req(data_fringe())
        dic <- data_fringe()$dic
        dateDic <- dic %>% dplyr::filter(hdType == "Dat")
        reqDate <-  trimws(setdiff(r$ckanConf$resource_priority_date %>% 
                                     stringr::str_split(pattern = ",") %>% 
                                     .[[1]], 
                                   c(NA, "")))
        
        if (nrow(dateDic) > 0) {
          d <- dateDic$id
          if (!identical(reqDate, character())) {
            d <-  unique(c(reqDate, dateDic$id))
          }
          d
        } else {
          return()
        }
      },
      error = function(cond) {
        return()
      }) 
    })
    
    dateRange <- reactive({
      tryCatch({
        req(dateToFilter())
        dateFormat <- "a_m_d"
        if (!is.null(r$ckanExtra$dateFormat)) dateFormat <- r$ckanExtra$dateFormat
        lNum <- 
          purrr::map(dateToFilter(), function(var){
            minNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MIN(",var,") FROM cdmxData"))
            maxNum <- DBI::dbGetQuery(r$ckanData, paste0("SELECT MAX(",var,") FROM cdmxData"))
            df <- data.frame(min = minNum[[1]], max = maxNum[[1]], id = var)
            df
          }) %>% dplyr::bind_rows()
        lNum
      },
      error = function(cond) {
        return()
      })      
    })
    
    
    
    coordinatesToPlot <- reactive({
      tryCatch({
        req(data_fringe())
        dic <- data_fringe()$dic
        GlnDic <- dic %>% dplyr::filter(hdType %in% "Gln")
        GltDic <- dic %>% dplyr::filter(hdType %in% "Glt")
        varCoor <- NULL
        if (nrow(GlnDic) == 0) return()
        if (nrow(GlnDic) > 0 & nrow(GltDic) > 0) varCoor <- c(GlnDic$id[1],GltDic$id[1])
        varCoor
      },
      error = function(cond) {
        return()
      })  
    })
    
    geoToPlot <- reactive({
      tryCatch({
        req(data_fringe())
        dic <- data_fringe()$dic
        GeoDic <- dic %>% dplyr::filter(hdType %in% c("Gcd", "Gnm"))
        if (nrow(GeoDic) == 0) GeoDic <- NULL
        GeoDic
      },
      error = function(cond) {
        return()
      })  
    })
    
    
    
    observe({
      r$d_sel <- data_select()
      r$dic_f <- dic_fringe()
      # print("in observeeer")
      # print(varsToFilter())
      r$vars_f <- varsToFilter()
      r$allCats <- catsToFilter()
      r$allNums <- numToFilter()
      r$numRange <- numRange()
      r$allDates <- dateToFilter()
      r$datesRange <- dateRange()
      r$coorToPlot <- coordinatesToPlot()
      r$geoToPlot <- geoToPlot()
    })
    
  })
}

## To be copied in the UI
# mod_selected_data_ui("selected_data_ui_1")

## To be copied in the server
# mod_selected_data_server("selected_data_ui_1")
