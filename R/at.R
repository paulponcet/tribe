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
#' mtcars %@% "class"
#' 
"%@%" <- 
function(x, 
         name) 
{
  attr(x, name, exact = TRUE)
}
