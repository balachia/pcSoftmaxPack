#' Inverse utility function for softmax market share
#'
#' Inverts two-sided terminal market share,
#' adjusting for option value of fleeing towards infinity.
#'
#' @param ms market share
#' @param a risk aversion adjustment (a < 1 -> more risk aversion) 
#' @export
u.inverse <- function(ms, a = 0.9) log(expm1(a*ms/2))

