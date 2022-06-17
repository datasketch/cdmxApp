#' Data selected
#'
#' @description A fct function when app works with differents data sets
#'
#' @return The return data according to question selected
#'
#' @noRd

filterTbl <-
  function(dataTbl, varToFilter, catsToView = NULL, filterNA = FALSE) {
    var <- dplyr::sym(varToFilter)
    df <- dataTbl
    dfNA <- NULL
    if (filterNA) dfNA <- df %>% dplyr::filter(is.na(!!var))
    if (!is.null(catsToView)) {
      df <- df %>% dplyr::filter(!!var %in% catsToView)
      if (filterNA)  df <- df %>% dplyr::union_all(dfNA)
    } else {
      if (filterNA) df <- dfNA
    }
    df
  }

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
    df <-   df %>% dplyr::collect()
    df$id[is.na(df$id)] <- "NA"
    df %>% 
      dplyr::mutate(label = paste0(as.character(id), " (", total, ")")) 
    
  }

selectTbl <-
  function(dataTbl, agg = "count", varToSel, varToGroup, varToAgg) {
    if (agg == "pctg") {
      if (!is.null(varToAgg)) {
        agg <- "sum"
        varToAgg <- dplyr::sym(varToAgg)
      } else {
        agg <- "count"
      }
    }
    print(varToSel)
    
    df <- dataTbl
    df <- df %>% 
      dplyr::select(!!varToSel) 
    
    varG <- dplyr::sym(varToGroup[1])
    
    df <- df %>% dplyr::group_by(!!varG)
    if (length(varToGroup) == 2) {
      varGadd <- dplyr::sym(varToGroup[2])
      df <- df %>% dplyr::group_by(!!varG, !!varGadd)
    }
    
    
    
    if (agg == "count") {
      df <- df %>%  dplyr::summarise(Conteo = dplyr::n())
    } else if (agg == "mean") {
      df <- df %>% dplyr::summarise(Promedio = mean(varToAgg, na.rm = TRUE))
    } else if (agg == "sum") {
      df <- df %>% dplyr::summarise(Total = sum(varToAgg, na.rm = TRUE))
    }
    df <-   df %>% dplyr::collect()
    df
    
  }