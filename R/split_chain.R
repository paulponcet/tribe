# Split a chain expression into its components.
#
# This function splits a chain of pipe calls into its components: its 
# left-hand side, a sequnce of right-hand sides, and the individual pipe
# components.
# 
# @param expr a non-evaluated pipe-line expression.
# @param env an environment in which to evaluate rhs parts.
# @return a list with components \code{lhs}, \code{rhss}, and \code{pipes}.
# @author Stefan Milton Bache and Hadley Wickham
split_chain <- function(expr, env) {
  # lists for holding the right-hand sides and the pipe operators.
  rhss  <- list()
  pipes <- list()

  # Process the call, splitting it at each valid magrittr pipe operator.
  i <- 1L 
  while(is.call(expr) && is_pipe(get(deparse(expr[[1L]])))) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]

    if (is_parenthesized(rhs))
      rhs <- eval(rhs, env, env)

    rhss[[i]] <- 
      if (is_function(rhs) || is_colexpr(rhs))
        prepare_function(rhs)
      else if (is_first(rhs)) 
        prepare_first(rhs)
      else 
        rhs

    # Make sure no anonymous functions without parentheses are used.
    if (is.call(rhss[[i]]) && identical(rhss[[i]][[1L]], quote(`function`)))
      stop("Anonymous functions must be parenthesized", call. = FALSE)

    expr <- expr[[2L]]
    i <- i + 1L
  }
  
  # return the components; expr will now hold the left-most left-hand side.
  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}
