#' Data selected
#'
#' @description A fct function when app works with differents data sets
#'
#' @return The return data according to question selected
#'
#' @noRd


# filterSql <-
#   function(dataSql, varToFilter, catsToView = NULL, filterNA = FALSE) {
#     
#   }


filterTbl <-
  function(dataTbl, varToFilter, catsToView = NULL, filterNA = FALSE) {
    var <- dplyr::sym(varToFilter)
    df <- dataTbl
    dfNA <- NULL
    catsToView <- setdiff(catsToView, "Todas")
    if (!identical(catsToView, character())) {
      if (filterNA) dfNA <- df %>% dplyr::filter(is.na(!!var))
      if (!is.null(catsToView)) {
        df <- df %>% dplyr::filter(!!var %in% catsToView)
        if (filterNA)  df <- df %>% dplyr::union_all(dfNA)
      } else {
        if (filterNA) df <- dfNA
      }
    }
    df
  }

filterNumTbl <- 
  function(dataTbl, varToFilter, rangeToView, originalRange) {
    df <- dataTbl
    if (!(rangeToView[1] == originalRange[1] & rangeToView[2] == originalRange[2])) {
      var <- dplyr::sym(varToFilter)
      if ( length(rangeToView) == 1) {
        df <- df %>% dplyr::filter(!!var >= !!rangeToView[1])
      }
      if ( length(rangeToView) == 2) {
        df <- df %>%  dplyr::filter(!!var >= !!rangeToView[1] & !!var <= !!rangeToView[2])
      }
    }
    df
  }




# filterDatTbl <-
#   function (dataTbl, varToFilter, rangeDate, originaDate) {
#     df <- dataTbl
#     var <- dplyr::sym(varToFilter)
#     
#     df <- df %>% dplyr::collect() 
#     df <- df %>% dplyr::mutate(Fechax = lubridate::ymd(!!var))
#     df <- df %>% 
#       tidyr::separate(Fechax, into = c("anio", "mes", "dia"), sep = "-", extra = "drop") 
#     df$Fechax <- paste0(df$anio, "-", df$mes)
#     df <- df %>% dplyr::select(-dia, -mes, -anio)
#     filterNumTbl(df, "Fechax", rangeDate, originaDate)
#   }


summaryTbl <- 
  function(dataTbl, agg = "conteo", aggregation = "sum", varToAgg, varToSumm = NULL) {
    var <- dplyr::sym(varToAgg)
    if (!is.null(varToSumm)) varToSumm <- dplyr::sym(varToSumm)
    df <- dataTbl
    df <- df %>%
      dplyr::group_by(id = !!var) 
    if (agg == "conteo") {
      df <- df %>%  dplyr::summarise(total = dplyr::n())
    } else {
      df <- df %>% dplyr::summarise(total = do.call(aggregation, list(varToSumm, na.rm = TRUE)))
    }
    df <- df %>% 
      dplyr::mutate(label = paste0(as.character(id), " (", total, ")")) 
    df
  }

selectTbl <-
  function(dataTbl, agg = "count", varToSel, varToGroup, 
           varToAgg, haveDate = FALSE, varDate, viz, dateFormat = "a_m_d") {
    
    
    if (is.null(dataTbl)) return()
    if (is.null(varToSel)) return()
    if (is.null(viz)) return()
    if (is.null(agg)) agg <- 'count'
    
    if (agg == "pctg") agg <- "count"
    df <- dataTbl

    df <- df %>% 
      dplyr::select(!!varToSel) 
    
   
    
    if (viz != "scatter") {
      if (is.null(varToGroup)) return()
      if (!is.null(varToAgg)) {
        if (length(varToAgg == 1)) varToAgg <- dplyr::sym(varToAgg[1])
      }
      
      varG <- dplyr::sym(varToGroup[1])
      df <- df %>% dplyr::group_by(!!varG)
      
      
      if (length(varToGroup) == 2) {
        varGadd <- dplyr::sym(varToGroup[2])
        df <- df %>% dplyr::group_by(!!varG, !!varGadd)
      }
    }
    
    if (!(viz %in% c("scatter", "map_bubbles", "map_heat"))) {
      if (agg == "count") {
        df <- df %>%  dplyr::summarise(Conteo = dplyr::n())
      } else if (agg == "mean") {
        df <- df %>% dplyr::summarise(Promedio = mean(!!varToAgg, na.rm = TRUE))
      } else if (agg == "sum") {
        df <- df %>% dplyr::summarise(Total = sum(!!varToAgg, na.rm = TRUE))
      }
    } 
    
    
    df <-   df %>% dplyr::collect()
    
    if (haveDate) {
      if (is.null(varDate)) return()
      df <- df %>% tidyr::drop_na(!!varDate)
      names(df) <- gsub("temporal_", "", names(df))
    }
    
    df
    
  }