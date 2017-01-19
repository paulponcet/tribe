
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
  #print(x)
  print(names(tribe(x)))
  obj <- attr(x, ".obj_stick")
  at <- attr(x, ".at_stick")
  #print(at)
  attr(x, ".obj_stick") <- NULL
  attr(x, ".at_stick") <- NULL
  #tribe(obj)[[at]] <- x
  attr(obj, at) <- x
  obj
}
