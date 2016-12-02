#' @title 
#' Attributes protection 
#' 
#' @description 
#' The function \code{shield} is made to facilitate 
#' the propagation  of attributes of an object \code{obj} 
#' through R operations. 
#' 
#' @param obj 
#' An object. 
#' 
#' @param at
#' A named list, the attributes to be possibly added to 
#' \code{obj}. 
#' 
#' @param propagate 
#' character. The method to be applied, one of \code{"all"}, \code{"most"}, 
#' \code{"some"}, \code{"none"}, \code{"many"}. 
#' 
#' If \code{propagate="some"} (the default), 
#' the attributes of \code{obj} are kept unchanged 
#' (up to the value of \code{keep_also}). 
#' 
#' If \code{propagate="all"} (not advised), 
#' the attributes of the returned object 
#' are exactly \code{at} (up to the value of \code{keep_also}). 
#' 
#' If \code{propagate="none"} (not advised either), 
#' the attributes of the returned object 
#' are \code{NULL} (up to the value of \code{keep_also}). 
#' 
#' If \code{propagate="most"}, new attributes taken from \code{at} 
#' will be added to \code{obj}; however, attributes found in \code{at} 
#' that have the same name as attributes of \code{obj} are not 
#' considered. 
#' 
#' @param keep_also 
#' character. A vector of named attributes to be added to 
#' the final result.  
#' 
#' @return 
#' The object \code{obj} with possibly different attributes. 
#' 
#' @importFrom base2 is.empty
#' @importFrom rlist list.merge
#' @export
#' 
#' @examples
#' library(dplyr)
#' df <- data.frame(x = sample(10, 5, rep = TRUE),
#'                  y = sample(10, 5, rep = TRUE)) %>%
#'   define(example="yes",
#'           package="dplyr", 
#'           class = c("my_tbl", "data.frame"))
#' tribe(df)
#' 
#' # Attributes are lost when the object passes through dplyr verbs
#' df2 <- df %>% 
#'   mutate(z = 3)
#' tribe(df2)
#' 
#' # Most attributes are kept
#' df3 <- shield(df2, tribe(df), propagate = "most")
#' tribe(df3)
#' 
#' # To keep the class, use 'keep_also'
#' df4 <- shield(df2, tribe(df), propagate = "most", keep_also = "class")
#' tribe(df4)
#' 
shield <- 
function(obj, 
         at, 
         propagate = "some", 
         keep_also = NULL)
{
  if (base2::is.empty(at)) return(obj)

  p <- propagate
  if (base2::is.empty(p)) p <- "some"
  stopifnot(is.character(p) && length(p) == 1L && p %in% c("many", "all", "most", "some", "none"))

  if (p == "all") {
    warning("the call 'propagate = 'all' should be used with care, see '?shield' for details")
    tribe(obj) <- at   # very dangerous
  } else if (p == "most") {
    tribe(obj) <- rlist::list.merge(at, tribe(obj))
  } else if (p == "none") {
    warning("the call 'propagate = 'none' should be used with care, see '?shield' for details")
    tribe(obj) <- NULL # a bit dangerous too
  } else if (p == "many") {
    tribe(obj) <- rlist::list.merge(tribe(obj), at)
  }
    
  if (!is.null(keep_also))
    tribe(obj) <- rlist::list.merge(tribe(obj), at[intersect(names(at), keep_also)])
  
  #for (i in seq_along(keep_also)) {
  #  attr(obj, keep_also[i]) = at[[keep_also[i]]]    
  #}
  obj
}
