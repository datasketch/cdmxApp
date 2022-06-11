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
    if (filterNA)  df <- df %>% dplyr::filter(is.na(!!var))
    if (!is.null(catsToView)) {
      df <- df %>% dplyr::filter(!!var %in% catsToView)
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
    df %>% 
      dplyr::mutate(label = paste0(id, " (", total, ")")) %>% 
      dplyr::collect()
  }


