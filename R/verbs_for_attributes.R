#' @title 
#' Manipulate attributes in a dplyr fashion
#'
#' @description 
#' The function \code{at_mutate} 
#' adds or changes attributes to \code{obj}. 
#' 
#' The function \code{at_select} 
#' selects attributes of \code{obj}, and removes the others. 
#' 
#' The function \code{at_rename} 
#' renames attributes of \code{obj}. 
#' 
#' The function \code{at_slice} 
#' chooses a specific attribute and returns it. 
#'
#' @param obj
#' An object. 
#' 
#' @param ...
#' Comma separated list of unquoted expressions.
#' 
#' @param .dots
#' Used to work around non-standard evaluation. 
#' 
#' @param at
#' Attribute to be obtained.
#' 
#' @return 
#' \code{at_slice} returns the attribute chosen. 
#' The other functions return \code{obj} with possibly modified attributes. 
#' 
#' @seealso \code{\link{structure}}, \code{\link{attributes}}
#' 
#' @importFrom lazyeval lazy_dots
#' @export
#' 
#' @examples
#' library(dplyr)
#' df <- data.frame(x = sample(10, 5, rep = TRUE),
#'                  y = sample(10, 5, rep = TRUE)) %>%
#'   at_mutate(example = "yes",
#'             package = "dplyr")
#' tribe(df)
#'
#' at_slice(df, names)
#' at_slice_(df, "class")
#' at_slice_(df, ~ package)
#'
#' df <- df %>%
#'   at_mutate_(package = ~ NULL,
#'              example = ~ "no")
#' tribe(df)
#' 
#' df <- df %>% 
#'   at_mutate_(.dots = list(x = ~ 2, y = ~ c(3,4)))
#' tribe(df)
#' 
at_mutate <-
function(obj,
         ...)
{
  at_mutate_(obj, .dots = lazyeval::lazy_dots(...))
}


#' @importFrom lazyeval all_dots
#' @export
#' @rdname at_mutate
#' 
at_mutate_ <-
function(obj,
         ...,
         .dots)
{
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  at_mutate_impl(obj, dots)
}


at_mutate_impl <-
function(obj,
         dots)
{
  if (length(dots) > 0) {
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", ".Label")
    replace <- c("dim", "dimnames", "names", "tsp", "levels")
    m <- match(names(dots), specials)
    ok <- (!is.na(m) & m)
    names(dots)[ok] <- replace[m[ok]]
    tribe(obj) <- c(tribe(obj), lazyeval::lazy_eval(dots))
  }
  obj
}


#' @importFrom lazyeval lazy_dots
#' @export
#' @rdname at_mutate
#' 
at_select <- 
function(obj, 
         ...)
{
  at_select_(obj, .dots = lazyeval::lazy_dots(...))
}


#' @importFrom dplyr select_
#' @export
#' @rdname at_mutate
#' 
at_select_ <-
function(obj, 
         ..., 
         .dots)
{
  tr <- tribe(obj)
  tribe(obj) <- dplyr::select_(tr, ..., .dots = .dots)
  obj
}


#' @importFrom lazyeval lazy_dots
#' @export
#' @rdname at_mutate
#' 
at_rename <- 
function(obj, 
         ...)
{
  at_rename_(obj, .dots = lazyeval::lazy_dots(...))
}


#' @importFrom dplyr rename_
#' @export
#' @rdname at_mutate
#' 
at_rename_ <- 
function(obj, 
         ..., 
         .dots)
{
  tr <- tribe(obj)
  tribe(obj) <- dplyr::rename_(tr, ..., .dots = .dots)
  obj
}


#' @export
#' @rdname at_mutate
#' 
at_slice <-
function(obj,
         at)
{
  sat <- substitute(at)
  if (is.name(sat)) {
    at_slice_(obj, at = deparse(sat))
  } else if (is.numeric(sat)) {
    at_slice_(obj, at = sat)
  }
}


#' @export
#' @rdname at_mutate
#' 
at_slice_ <-
function(obj,
         at)
{
  if (inherits(at, "formula")) {
    at <- at[[2L]]
    at <- deparse(substitute(at))
  }
  #attr(obj, at, exact = TRUE)
  tribe(obj)[[at]]
}
