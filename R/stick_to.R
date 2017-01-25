#' @title 
#' Work on a specific attribute within a pipeline
#' 
#' @description 
#' The functions \code{stick_to} and \code{unstick} enable 
#' to select an attribute within a pipe and work on it. 
#' 
#' It must be combined with the \code{\%@@>\%} pipe to work properly, 
#' see the example below. 
#' 
#' @param obj
#' An object with an \code{at} attribute. 
#' 
#' @param at
#' The name of the attribute to be considered. 
#' 
#' @param x
#' An object to be unsticked. 
#' Must have \code{".obj_stick"} and \code{".at_stick"} attributes. 
#' 
#' @return 
#' \code{stick_to} basically inverses the roles of \code{.data} and \code{at}, 
#' meaning that \code{.data} becomes an attribute of the selected attribute. 
#' \code{unstick} makes the inverse operation. 
#' 
#' @export
#' 
#' @examples 
#' library(dplyr)
#' library(observer)
#' 
#' df <- ggplot2::diamonds %>% 
#'   mutate(depth2 = 100*2*z/(x+y)) %>% 
#'   observe_if(abs(depth-depth2) < 1)
#' 
#' observations(df)
#' 
#' df %>% 
#'   stick_to(observations) %@>% 
#'   mutate(Id = 2) %@>% 
#'   select(Id, Status) %>% 
#'   unstick()
#' 
#' observations(df)
#' 
stick_to <- 
function(obj, at)
{
  sat <- deparse(substitute(at))
  stick_to_(obj, sat)
}


#' @export
#' @rdname stick_to
#' 
stick_to_ <- 
function(obj, at)
{
  if (inherits(at, "formula")) {
    at <- at[[2L]]
    at <- deparse(substitute(at))
  }
  x <- attr(obj, at) 
  define_(x, 
          .obj_stick = ~ obj, 
          .at_stick = ~ at)
}


#' @export
#' @rdname stick_to
#' 
unstick <-
function(x)
{
  obj <- attr(x, ".obj_stick")
  at <- attr(x, ".at_stick")
  attr(x, ".obj_stick") <- NULL
  attr(x, ".at_stick") <- NULL
  attr(obj, at) <- x
  obj
}
