#' Total market share across the number line
#'
#' @param v0s value of focal producer at their location (vectorized)
#' @param x0 location of focal producer
#' @param vgrid values of other producers at their location
#' @param xgrid locations of other producers
#' @export
ms.total <- function(v0s, x0, vgrid, xgrid) {
    stopifnot(length(vgrid) == length(xgrid))
    stopifnot(length(x0) == 1)
    # sort positions
    ord <- order(c(x0, xgrid))
    # build and augment intervals; xg = sorted positions grid
    xgaug <- c(-Inf, c(x0, xgrid)[ord], Inf)
    # sequence along intervals
    ninterval <- length(xgaug) - 1
    #mss <- sapply(seq_len(ninterval), function(itl) {
    mss <- vapply(X = seq_len(ninterval),
                  FUN.VALUE = array(0.0, dim = c(length(v0s))),
                  FUN = function(itl) {
        # get basic interval characteristics
        xl <- xgaug[itl]
        xr <- xgaug[itl + 1]
        itl.length <- xr - xl
        # accomodate zero-length intervals
        if(itl.length > 0) {
            # convert left/right to near/far orientation on each interval
            side.below <- xl < x0
            if(xl < x0) {
                xnear <- xr
                xfar <- xl
                which.inc <- xgrid < xnear
            } else {
                xnear <- xl
                xfar <- xr
                which.inc <- xgrid > xnear
            }
            # now calculate distances and values? want distances to *near* pole
            vs.itl <- vgrid - abs(xgrid - xnear)
            v0s.itl <- v0s - abs(x0 - xnear)
            # calculate market share
            if(is.infinite(xfar)) {
                ms <- mstv(v0s.itl, vs.itl)
            } else {
                ms <- msiv(v0s.itl, vds = vs.itl[!which.inc], vis = vs.itl[which.inc], d = itl.length)
            }
        } else {
            ms <- rep(0, length(v0s))
        }
        ms
    })
    mss <- array(mss, dim = c(length(v0s), ninterval))
    ms.sum <- apply(mss, 1, sum)
    list(ms = ms.sum, mss = mss)
}

#' Total market share across the number line (truncated)
#'
#' @param v0s value of focal producer at their location (vectorized)
#' @param x0 location of focal producer
#' @param vgrid values of other producers at their location
#' @param xgrid locations of other producers
#' @export
ms.total.trunc <- function(v0s, x0, vgrid, xgrid, trunc = 1e-2) {
    trunc.d <- max(v0s - log( ((1 + exp(v0s)) ^ trunc) - 1 ))
    #print(trunc.d)
    stopifnot(length(vgrid) == length(xgrid))
    stopifnot(length(x0) == 1)
    # sort positions
    ord <- order(c(x0, xgrid))
    # build and augment intervals; xg = sorted positions grid
    xgaug0 <- c(x0, xgrid)[ord]
    trunc.range <- (xgaug0 < x0 + trunc.d) & (xgaug0 > x0 - trunc.d)
    xgaug <- xgaug0[trunc.range]
    if(trunc.range[1]) xgaug <- c(-Inf, xgaug)
    if(trunc.range[length(xgaug0)]) xgaug <- c(xgaug, Inf)
    #xgaug <- c(-Inf, c(x0, xgrid)[ord], Inf)
    # sequence along intervals
    ninterval <- length(xgaug) - 1
    #mss <- sapply(seq_len(ninterval), function(itl) {
    mss <- vapply(X = seq_len(ninterval),
                  FUN.VALUE = array(0.0, dim = c(length(v0s))),
                  FUN = function(itl) {
        # get basic interval characteristics
        xl <- xgaug[itl]
        xr <- xgaug[itl + 1]
        itl.length <- xr - xl
        # accomodate zero-length intervals
        if(itl.length > 0) {
            # convert left/right to near/far orientation on each interval
            side.below <- xl < x0
            if(xl < x0) {
                xnear <- xr
                xfar <- xl
                which.inc <- xgrid < xnear
            } else {
                xnear <- xl
                xfar <- xr
                which.inc <- xgrid > xnear
            }
            # now calculate distances and values? want distances to *near* pole
            vs.itl <- vgrid - abs(xgrid - xnear)
            v0s.itl <- v0s - abs(x0 - xnear)
            # calculate market share
            if(is.infinite(xfar)) {
                ms <- mstv(v0s.itl, vs.itl)
            } else {
                ms <- msiv(v0s.itl, vds = vs.itl[!which.inc], vis = vs.itl[which.inc], d = itl.length)
            }
        } else {
            ms <- rep(0, length(v0s))
        }
        ms
    })
    mss <- array(mss, dim = c(length(v0s), ninterval))
    ms.sum <- apply(mss, 1, sum)
    list(ms = ms.sum, mss = mss)
}

ms.total <- ms.total.trunc
