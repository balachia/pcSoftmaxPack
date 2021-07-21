#' Argument to atan(h) functions
#'
#' @param kd value at focal pole of decreasing items
#' @param ki value at focal pole of increasing items
#' @param d length of interval
arg.atanx <- function(kd, ki = kd, d = 1) (expm1(d)) / (1 + 2*kd + (1 + 2*ki) * exp(d))

#' Market Share - Interval; k (exponentiated) formulation
#'
#' @param kd value at focal pole of decreasing items
#' @param ki value at focal pole of increasing items
#' @param d length of interval
msik.baseline <- function(kd, ki = kd, d = 1) {
    # vectorize arguments
    vargs <- vectorize.args(kd = kd, ki = ki, d = d)
    kd <- vargs$kd
    ki <- vargs$ki
    d <- vargs$d
    # discriminant: positive = arctan; negative = arctanh
    disc <- 4*kd*ki - 1
    # universal arctan(h) argument constructor
    base <- sqrt(abs(disc))
    arg <- arg.atanx(kd, ki, d)
    # construct components
    suppressMessages({
        c.atan <- dplyr::if_else(abs(disc) < .Machine$double.eps,
            -arg,
            dplyr::if_else(disc > 0,
                -atan(base * arg) / base,
                -atanh(base * arg) / base))
    })
    c.d <- 0.5*d
    c.logp <- 0.5*log1p(kd + ki)
    c.logn <- -0.5*log1p(kd*exp(-d) + ki*exp(d))
    # add them in an order that hopefully minimizes catastrophic cancellation
    c.atan + (c.d + (c.logp + c.logn))
}

#' Market Share - Interval; k (exponentiated) formulation; naive mpfr-wrapped
#'
#' @param kd value at focal pole of decreasing items
#' @param ki value at focal pole of increasing items
#' @param d length of interval
msik.baseline.mpfr.wrap <- function(kd, ki = kd, d = 1, prec = 1024) {
    msik.baseline(mpfr(kd, prec), mpfr(ki, prec), mpfr(d, prec))
}

#' Market Share - Interval; k (exponentiated) formulation; fully inside mpfr land
#'
#' @param kd value at focal pole of decreasing items
#' @param ki value at focal pole of increasing items
#' @param d length of interval
msik.full.mpfr <- function(kd, ki = kd, d = 1, prec = 1024) {
    # vectorize arguments
    vargs <- vectorize.args(kd = kd, ki = ki, d = d)
    kd.m <- mpfr(vargs$kd, prec)
    ki.m <- mpfr(vargs$ki, prec)
    d.m <- mpfr(vargs$d, prec)
    # keep certain things outside mpfr
    # discriminant: positive = arctan; negative = arctanh
    disc <- 4 * vargs$kd * vargs$ki - 1
    #disc <- 4 * kd.m * ki.m - 1
    # universal arctan(h) argument constructor
    base <- sqrt(abs(disc))
    #base <- mpfr(abs(disc), prec)
    arg <- arg.atanx(kd.m, ki.m, d.m)
    suppressMessages({
        c.atan <- dplyr::if_else(abs(disc) < .Machine$double.eps,
            -arg,
            dplyr::if_else(disc > 0,
                -atan(base * arg) / base,
                -atanh(base * arg) / base))
    })
    # add them in an order that hopefully minimizes catastrophic cancellation
    c.atan + 0.5*(d.m + (log1p(kd.m + ki.m) - log1p(kd.m*exp(-d.m) + ki.m*exp(d.m))) )
}

#msik.rcpp <- function(kd, ki = kd, d = 1, prec = 1024)

#' Market Share - Interval; k (exponentiated) formulation
#'
#' @param kd value at focal pole of decreasing items
#' @param ki value at focal pole of increasing items
#' @param d length of interval
#' @importFrom Rmpfr mpfr asNumeric
#' @export
msik <- function(kd, ki, d, prec = 1024) {
    #asNumeric(msik.baseline(mpfr(kd, prec), mpfr(ki, prec), mpfr(d, prec)))
    asNumeric(msik.full.mpfr(kd, ki, d, prec))
}

#' Market Share - Interval; v (base-quality) formulation
#' 
#' By construction, v0 (company of interest) is always in the decreasing set
#'
#' @param v0s value of focal producer at near endpoint (vectorized) 
#' @param vds value of increasing producers at near endpoint (producers with value decreasing in distance)
#' @param vis value of increasing producers at near endpoint (producers with value increasing in distance)
#' @param d length of interval
#' @export
msiv <- function(v0s, vds, vis, d) {
    ss <- relative.share(v0s, vds)
    kd <- exp(v0s) + sum(exp(vds))
    ki <- sum(exp(vis))
    #mss <- msik(kd = exp(v0s) + sum(exp(vds)),
    #            ki = sum(exp(vis)),
    #            d = d)
    mss <- msik(kd = kd,
                ki = ki,
                d = d)
    ss * mss
}

