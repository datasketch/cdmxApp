#' Question's buttons
#'
#' @description A util function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

question_buttons <- function(ids = NULL, labels = NULL, ...) {
  if (is.null(ids)) stop("Please enter identifiers for each question")
  if (is.null(labels)) stop("Please enter labels for each question")
  
  df <- data.frame(id = ids, questions = labels)
  l <- purrr::map(1:nrow(df), function(z){
    shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed")
  })
  l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
  l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))
  
  l
}

