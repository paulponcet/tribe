#' @title 
#' Manipulate attributes in a dplyr fashion
#'
#' @description 
#' The function \code{define} (synonym: \code{at_mutate}) 
#' adds or changes attributes to \code{obj}. 
#' 
#' The function \code{keep} (synonym: \code{at_select}) 
#' selects attributes of \code{obj}, and removes the others. 
#' 
#' The function \code{rebrand} (synonym: \code{at_rename}) 
#' renames attributes of \code{obj}. 
#' 
#' The function \code{take} (synonym: \code{at_slice}) 
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
#' \code{take} returns the attribute chosen. 
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
#'   define(example="yes",
#'          package="dplyr")
#' tribe(df)
#'
#' take(df, names)
#' take_(df, "class")
#' take_(df, ~ package)
#'
#' df <- df %>%
#'   define_(package = ~ NULL,
#'           example = ~ "no")
#' tribe(df)
#' 
#' df <- df %>% 
#'   define_(.dots = list(x =~ 2, y =~ c(3,4)))
#' tribe(df)
#' 
define <-
function(obj,
         ...)
{
  define_(obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname define
#' 
at_mutate <- define

#' @importFrom lazyeval all_dots
#' @export
#' @rdname define
#' 
define_ <-
function(obj,
         ...,
         .dots)
{
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  define_impl(obj, dots)
}

#' @export
#' @rdname define
#' 
at_mutate_ <- define_


define_impl <-
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
  return(obj)
}


#' @importFrom lazyeval lazy_dots
#' @export
#' @rdname define
#' 
keep <- 
function(obj, 
         ...)
{
  keep_(obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname define
#' 
at_select <- keep


#' @importFrom dplyr select_
#' @export
#' @rdname define
#' 
keep_ <-
function(obj, 
         ..., 
         .dots)
{
  tr <- tribe(obj)
  tribe(obj) <- dplyr::select_(tr, ..., .dots = .dots)
  obj
}

#' @export
#' @rdname define
#' 
at_select_ <- keep_


#' @importFrom lazyeval lazy_dots
#' @export
#' @rdname define
#' 
rebrand <- 
function(obj, 
         ...)
{
  rebrand_(obj, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname define
#' 
at_rename <- rebrand


#' @importFrom dplyr rename_
#' @export
#' @rdname define
#' 
rebrand_ <- 
function(obj, 
         ..., 
         .dots)
{
  tr <- tribe(obj)
  tribe(obj) <- dplyr::rename_(tr, ..., .dots = .dots)
  obj
}

#' @export
#' @rdname define
#' 
at_rename_ <- rebrand_


#' @export
#' @rdname define
#' 
take <-
function(obj,
         at)
{
  sat <- substitute(at)
  if (is.name(sat)) {
    take_(obj, at = deparse(sat))
  } else if (is.numeric(sat)) {
    take_(obj, at = sat)
  }
}

#' @export
#' @rdname define
#' 
at_slice <- take


#' @export
#' @rdname define
#' 
take_ <-
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

#' @export
#' @rdname define
#' 
at_slice_ <- take_
