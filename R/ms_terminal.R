
#' Market Share, Terminal, k (exponentiated) formula
#' 
#' @param k sum of exponentiated market values
mstk <- function(k) log1p(k)

#' Market Share, Terminal, v (value) formula
#' 
#' @param v0s value of focal producer at terminal pole (vectorized)
#' @param vs value of other producers at terminal pole
#' @export
mstv <- function(v0s, vs) relative.share(v0s, vs) * mstk(sum(exp(vs)) + exp(v0s))
