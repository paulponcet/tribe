# Check whether a symbol is a valid pipe.
# 
# @param pipe A quoted symbol
# @return logical - TRUE if a valid pipe, FALSE otherwise.
is_pipe <- function(pipe) {
  is.function(pipe) && 
    inherits(pipe, "pipe") && 
    identical(names(formals(pipe)), c("lhs", "rhs"))
}

# Determine whether an non-evaluated call is parenthesized
#
# @param a non-evaluated expression
# @retun logical - TRUE if expression is parenthesized, FALSE otherwise.
# @author Stefan Milton Bache and Hadley Wickham
is_parenthesized <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`(`))
}


# Check whether a pipe is the compound assignment pipe operator
#
# @param pipe A (quoted) pipe
# @return logical - TRUE if pipe is the compound assignment pipe,
#   otherwise FALSE.
# @author Stefan Milton Bache and Hadley Wickham; '%<at>%' added by Paul Poncet
is_compound_pipe <- function(pipe) {
  identical(pipe, quote(`%<>%`)) ||
  identical(pipe, quote(`%<at>%`))
}

# Check whether expression is enclosed in curly braces.
#
# @param  expr An expression to be tested.
# @return logical - TRUE if expr is enclosed in `{`, FALSE otherwise.
# @author Stefan Milton Bache and Hadley Wickham
is_funexpr <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`{`))
}

# Check whether expression has double or triple colons
#
# @param  expr An expression to be tested.
# @return logical - TRUE if expr contains `::` or `:::`, FALSE otherwise.
# @author Stefan Milton Bache and Hadley Wickham
is_colexpr <- function(expr) {
  is.call(expr) &&
    (identical(expr[[1L]], quote(`::`)) || identical(expr[[1L]], quote(`:::`)))
}

# Check whether a symbol is the magrittr placeholder.
#
# @param  symbol A (quoted) symbol
# @return logical - TRUE if symbol is the magrittr placeholder, FALSE otherwise.
# @author Stefan Milton Bache and Hadley Wickham
is_placeholder <- function(symbol) {
  identical(symbol, quote(.))
}
