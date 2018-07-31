#' @title 
#' Infix attribute accessor
#' 
#' @description 
#' Infix attribute accessor
#' 
#' @param x
#' Object 
#' 
#' @param name
#' Attribute name
#' 
#' @author 
#' Borrowed from Hadley Wickham's \pkg{purrr} package. 
#' 
#' @export
#' @name at
#' 
#' @examples 
#' factor(1:3) %@% "levels"
#' factor(1:3) %@% levels
#' 
#' mtcars %@% "class"
#' mtcars %@% class
#' 
"%@%" <- 
function(x, 
         name)
{
  name <- deparse(substitute(name))
  n <- nchar(name)
  if (substr(name, 1L, 1L) == "\"") {
    name <- substring(name, 2L)
    n <- n-1L
  }
  if (substr(name, n, n) == "\"") {
    name <- substring(name, 1L, n-1L)
  }
  attr(x, name, exact = TRUE)
}
