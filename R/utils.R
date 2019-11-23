
is_empty <- function(x) {
  length(x) == 0L
}


#' @importFrom dplyr lst
#' 
nlist <- function(...) {
  x <- dplyr::lst(...)
  if (is_empty(x)) {
    names(x) <- character(0L)
  }
  x
}
