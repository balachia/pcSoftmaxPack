Eumsi.optim.novec <- function(x0, vgrid, xgrid, ...) {
    # get mean/variance
    msd <- wiener.moments(x0 = x0, xgrid = xgrid, vgrid = vgrid)
    Eums.uinverse(x0 = x0, vgrid = vgrid, xgrid = xgrid, mean = msd$mean, sd = msd$sd, ...)
}

#' Optimization target for expected utility (inverse utility) of market share
#'
#' @importFrom fastGHQuad gaussHermiteData
#' @export
Eumsi.optim.vec <- function(x0s, gh.rule = fastGHQuad::gaussHermiteData(30), ...) {
    sapply(x0s, Eumsi.optim.novec, gh.rule = gh.rule, ...)
}

#' Find optimal entry location on terminal intervals
#'
#' @param x location of terminus
#' @param sgn search in positive or negative direction?
#' @param last.optim best guess is last optimum... (4 seems to be good baseline for a 0,0 start)
Eumsi.optimize.terminal <- function(x, sgn = 1, last.optim = 4, ...) {
    limit <- sgn + c(-1, 1)
    index <- (sgn + 3)/2
    limit <- limit*abs(last.optim)
    # exponential backoff
    tol <- .Machine$double.eps^0.25
    repeat {
        res <- optimize(Eumsi.optim.vec, interval = c(x, x) + limit, maximum = TRUE, tol = tol, ...)
        if(abs(limit[index] - res$maximum) < 2*tol) {
            cat(sprintf('expanding %d to %e\n', sgn, limit[index]))
            limit <- limit * 2
        } else {
            break
        }
    }
    res
}

#' Find optimal entry location per expected inverse utility
#'
#' @param vgrid values of other competitors
#' @param xgrid positions of other competitors
#' @param term.last.optim locations of last optima at terminals
#' @param gh.rule Gauss-Hermite rule to use across intergrations
#' @export
Eumsi.optimize <- function(vgrid, xgrid, term.last.optim = c(-4, 4), gh.rule = fastGHQuad::gaussHermiteData(30)) {
    x.joint <- sort(unique(xgrid))
    x.aug <- c(-Inf, x.joint, Inf)
    # evaluate joints
    x.joint <- unique(xgrid)
    #print(x.joint)
    eus.joint <- Eumsi.optim.vec(x0s = x.joint, xgrid = xgrid, vgrid = vgrid, gh.rule = gh.rule)
    # evaluate intervals
    res.itls <- lapply(seq(from = 1, length.out = length(x.joint) - 1), function(itl) {
        #print(x.joint[itl + (0:1)])
        optimize(Eumsi.optim.vec, interval = x.joint[itl + (0:1)], maximum = TRUE,
                 gh.rule = gh.rule, vgrid = vgrid, xgrid = xgrid)
    })
    # evaluate terminals
    res.l <- Eumsi.optimize.terminal(x = x.joint[1],
                                     gh.rule = gh.rule, sgn = -1, last.optim = term.last.optim[1],
                                     xgrid = xgrid, vgrid = vgrid)
    res.r <- Eumsi.optimize.terminal(x = x.joint[length(x.joint)],
                                     gh.rule = gh.rule, sgn = 1, last.optim = term.last.optim[2],
                                     xgrid = xgrid, vgrid = vgrid)
    #res.optims <- purrr::transpose(c(res.itls, res.l, res.r))
    res.optims <- (c(res.itls, list(res.l), list(res.r))) %>%
        purrr::transpose() %>%
        lapply(unlist)
    eus <- c(eus.joint, res.optims$objective)
    xs <- c(x.joint, res.optims$maximum)
    term.optim <- c(res.l$maximum, res.r$maximum)
    which.optim <- which.max(eus)
    #list(eus.joint, intervals = res.itls, left = res.l, right = res.r)
    list(maximum = xs[which.optim], objective = eus[which.optim], term.optim = term.optim, xs = xs, eus = eus)
}
