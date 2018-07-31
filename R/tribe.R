#' @title 
#' Object attribute list
#' 
#' @description 
#' The function \code{tribe} is identical to \code{\link[base]{attributes}}, 
#' expect that it always returns a named list (thus, when \code{attributes} 
#' will return \code{NULL}, \code{tribe} will return an empty named list). 
#' 
#' @param obj
#' An object.
#' 
#' @param x
#' A list (of attributes) to be \code{untribe}d. 
#' 
#' @param keep_obj
#' logical. If \code{TRUE}, \code{obj} is passed as an attribute to 
#' the result (useful in combination of \code{untribe}). 
#' 
#' @param value
#' An appropriate named list of attributes, or \code{NULL}. 
#' 
#' @return 
#' A named list, the attributes of \code{obj}. 
#' 
#' @seealso 
#' \code{\link[base]{attributes}}, 
#' \code{\link[base]{attributes<-}}, 
#' \code{\link[base]{mostattributes<-}}. 
#' 
#' @importFrom bazar nlist
#' @export
#' 
#' @examples 
#' library(lplyr)
#' A <- c(x = 1, y = 2, z = 3) %>% 
#'   at_mutate(package = "trib?")
#' A %>% 
#'   tribe(keep_obj = TRUE) %@>% 
#'   mutate(package = "tribe") %>% 
#'   untribe()
#' 
tribe <- 
function(obj, 
         keep_obj = FALSE)
{
  at <- attributes(obj)
  if (is.null(at)) {
    at <- bazar::nlist()
  }
  if (keep_obj) {
    #attributes(obj) <- NULL
    attr(at, ".obj_tribe") <- obj
  }
  at
}


#' @importFrom bazar is.empty
#' @export
#' @rdname tribe
#' 
"tribe<-" <- 
function(obj, value)
{
  attributes(obj) <- if (bazar::is.empty(value)) NULL else value
  obj
}


#' @export
#' @rdname tribe
#' 
untribe <- 
function(x)
{
  obj <- attr(x, ".obj_tribe")
  attr(x, ".obj_tribe") <- NULL
  attributes(obj) <- x
  obj
}
