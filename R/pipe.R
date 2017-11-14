#' @title 
#' Create a pipe operator.
#
#' @description
#' This function is used to create magrittr like pipe operators.
#' 
#' @param propagate
#' character. See the eponymous argument in \code{\link[tribe]{shield}}. 
#' 
#' @param keep_also
#' character. See the eponymous argument in \code{\link[tribe]{shield}}. 
#' 
#' @param lhs 
#' Left-hand side of the pipe. 
#' 
#' @param rhs
#' Right-hand side of the pipe. 
#' 
#' @param try
#' logical. If \code{TRUE} and the pipe \code{x > f} generates an error, 
#' then the pipe \code{x try> f} returns \code{x} unchanged silently. 
#' 
#' @author Stefan Milton Bache and Hadley Wickham for the 
#' original \code{pipe} function in package \pkg{magrittr}; 
#' Paul Poncet for the modifications introduced. 
#' 
#' @seealso \code{\link{shield}} in this package. 
#' 
#' @importFrom magrittr freduce
# #' @importFrom tribe shield
#' @export
#' 
#' @examples 
#' library(dplyr)
#' df <- data.frame(x = sample(10, 5, rep = TRUE),
#'                  y = sample(10, 5, rep = TRUE)) %>%
#'   define(example="yes",
#'          package="dplyr", 
#'          class = c("my_tbl", "data.frame"))
#' tribe(df)
#' 
#' # Attributes just created are lost when the object 
#' # passes through dplyr verbs
#' tribe(df %>% mutate(z=3))
#' 
#' # With the pipe '%@>%', most attributes are kept
#' tribe(df %@>% mutate(z=3))
#' 
#' # One can create a new pipe to adjust attributes propagation settings
#' "%newpipe>%" <- make_pipe(propagate="none", keep_also = "example")
#' tribe(df %newpipe>% mutate(z=3))
#' 
make_pipe <- 
function(propagate, # = getOption("propagate") 
         keep_also = NULL, 
         try = FALSE)
{
  pipe <- function(lhs, rhs)
  {
    # the parent environment
    parent <- parent.frame()
    
    # the environment in which to evaluate pipeline
    env    <- new.env(parent = parent)

    # split the pipeline/chain into its parts.
    chain_parts <- split_chain(match.call(), env = env)

    pipes <- chain_parts[["pipes"]] # the pipe operators.
    rhss  <- chain_parts[["rhss" ]] # the right-hand sides.
    lhs   <- chain_parts[["lhs"  ]] # the left-hand side.

    # Create the list of functions defined by the right-hand sides.
    env[["_function_list"]] <- 
      lapply(seq_along(rhss), 
             function(i) wrap_function(rhss[[i]], pipes[[i]], parent))

    # Create a function which applies each of the above functions in turn.
    env[["_fseq"]] <-
     `class<-`(eval(quote(function(value) { 
         magrittr::freduce(value, `_function_list`) 
       }), 
                    env, env), c("fseq", "function"))
 
    # make freduce available to the resulting function 
    # even if magrittr is not loaded.
    env[["freduce"]] <- magrittr::freduce 
    
    # Result depends on the left-hand side.
    if (is_placeholder(lhs)) {
      # return the function itself.
      f <- env[["_fseq"]]
      if (try) {
        function(.) {
          at <- attributes(.)
          tryCatch(shield(f(.), at, 
                          propagate = propagate, keep_also = keep_also), 
                   error = function(e) .)
        }
      } else {
        function(.) {
          at <- attributes(.)
          shield(f(.), at, propagate = propagate, keep_also = keep_also)
        }
      }
      
    } else {
      # evaluate the LHS
      env[["_lhs"]] <- eval(lhs, parent, parent)
      at <- attributes(env[["_lhs"]])
      
      # compute the result by applying the function to the LHS
      if (try) {
        result <- tryCatch(withVisible(eval(quote(`_fseq`(`_lhs`)), env, env)), 
                           error = function(e) {
                             withVisible(eval(quote(`_lhs`), env, env))
                           })
      } else {
        result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      }
      #print(names(tribe(result[["value"]])))
      value  <- shield(result[["value"]], at, 
                       propagate = propagate, keep_also = keep_also)
      
      # If compound assignment pipe operator is used, assign result
      if (is_compound_pipe(pipes[[1L]])) {
        eval(call("<-", lhs, value), parent, parent)
      # Otherwise, return it.
      } else {
        if (result[["visible"]]) 
          value
        else 
          invisible(value)
      }
    }
  }
  class(pipe) <- c("pipe", "function")
  return(pipe)
}


#' @export
#' @rdname make_pipe
#' 
`%@>%` <- make_pipe(propagate = "most", keep_also = "class")


#' @export
#' @rdname make_pipe
#' 
`%<@>%` <- make_pipe(propagate = "most", keep_also = "class")


#' @export
#' @rdname make_pipe
#' 
`%try>%` <- make_pipe(propagate = "some", try = TRUE)

