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
#' @importFrom base2 namedlist
#' @export
#' 
tribe <- 
function(obj)
{
  at <- attributes(obj)
  if (is.null(at)) {
    return(base2::namedlist())
  }
  at
}


#' @importFrom base2 is.empty
#' @export
#' @rdname tribe
#' 
"tribe<-" <- 
function(obj, value)
{
  attributes(obj) <- if (base2::is.empty(value)) NULL else value
  obj
}
