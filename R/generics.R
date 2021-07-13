#' Convert arguments to vectors of equal length
#' 
#' @param ... ensures that all arguments have same length, expanding vectors of length 1
vectorize.args <- function(...) {
    vecs <- list(...)
    # vectorize arguments
    ls <- lapply(vecs, length)
    lmax <- do.call(max, ls)
    if(lmax > 1) {
        rep1 <- function(x, lx) {
            if(lx > 1 && lx < lmax) {
                stop(paste0("all arguments must have length 1 or ", lmax))
            }
            #stopifnot(lx == 1 || lx == lmax)
            res <- rep(x, length.out = lmax)
        }
        # process one at a time
        mapply(rep1, x = vecs, lx = ls, SIMPLIFY = FALSE)
    } else {
        vecs
    }
}

############################################################

#' Relative share of output
#' 
#' @param v0s value of focal producer at this pole (vectorized)
#' @param vs values of other same-direction competition producers from this pole
relative.share <- function(v0s, vs) exp(v0s) / (exp(v0s) + sum(exp(vs)))

#' Find bracketing points within a grid
#'
#' Given a focal point, finds the positions that bracket it within a grid,
#' possibly equal at left endpoint (i.e. closer to -Inf)
#'
#' @param x0 focal point
#' @param xgrid grid to search through
#' @return list with `l`eft and `r`ight endpoints (or NA for open intervals)
bounds.in.grid <- function(x0, xgrid) {
    o <- order(xgrid)
    nless <- sum(x0 >= xgrid)
    list(l = if(nless > 0) o[nless] else NA_integer_, r = o[nless+1])
}

############################################################
# Brownian motion rules

#' Open interval standard deviation, vectorized
#' 
#' @param x0 focal point
#' @param xl left endpoint of interval (NA for -Inf)
#' @param xr right endpoint of interval (NA for Inf)
open.sd <- function(x0, xl, xr) {
    sqrt(abs(x0 - if_else(is.na(xr), xl, xr)))
}

#' Bridge interval mean
#' 
#' @param x0 focal point
#' @param xl left endpoint of interval
#' @param xr right endpoint of interval
#' @param vl value at left endpoint of interval
#' @param vr value at right endpoint of interval
bridge.mean <- function(x0, xl, xr, vl, vr) {
    vl + (vr - vl) * ((x0 - xl) / (xr - xl))
}

#' Bridge interval standard deviation
#' 
#' @param x0 focal point
#' @param xl left endpoint of interval
#' @param xr right endpoint of interval
bridge.sd <- function(x0, xl, xr) {
    sqrt(((x0 - xl) * (xr - x0)) / (xr - xl))
}

wiener.moments <- function(x0, xgrid, vgrid) {
    # get bounds
    lr <- bounds.in.grid(x0, xgrid)
    # check if terminal
    if(is.na(lr$l)) {
        # left terminal, valid endpoint at right
        ms.mean <- vgrid[lr$r]
        ms.sd <- sqrt(xgrid[lr$r] - x0)
    } else if(is.na(lr$r)) {
        # right terminal, valid endpoint at left
        ms.mean <- vgrid[lr$l]
        ms.sd <- sqrt(x0 - xgrid[lr$l])
    } else {
        # bridge
        xl <- xgrid[lr$l]
        xr <- xgrid[lr$r]
        vl <- vgrid[lr$l]
        vr <- vgrid[lr$r]
        ms.mean <- vl + (vr - vl) * ((x0 - xl) / (xr - xl))
        ms.sd <- sqrt(((x0 - xl) * (xr - x0)) / (xr - xl))
    }
    list(mean = ms.mean, sd = ms.sd)
}


#bridge.mean <- function(x0, xgrid, vgrid) {
#    lr <- bounds.in.grid(x0, xgrid)
#    xl <- xgrid[lr$l]
#    xr <- xgrid[lr$r]
#    vl <- vgrid[lr$l]
#    vr <- vgrid[lr$r]
#    vl + (vr - vl) * ((x0 - xl) / (xr - xl))
#}

#bridge.sd <- function(x0, xgrid) {
#    lr <- bounds.in.grid(x0, xgrid)
#    xl <- xgrid[lr$l]
#    xr <- xgrid[lr$r]
#    sqrt(((x0 - xl) * (xr - x0)) * (xr - xl))
#}
