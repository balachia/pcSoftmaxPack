#' Gauss-Hermite reparametrization
ms.total.gh <- function(zs, x0, vgrid, xgrid, mean = 0, sd = 1) {
    ms.total(v0s = sqrt(2) * sd * zs + mean, x0 = x0, vgrid = vgrid, xgrid = xgrid)$ms / sqrt(pi)
}

Ems <- function(..., gh.rule = fastGHQuad::gaussHermiteData(30)) {
    sum(gh.rule$w * ms.total.gh(zs = gh.rule$x, ...))
}

Eums.uinverse <- function(..., gh.rule = fastGHQuad::gaussHermiteData(30), a = 0.9) {
    sum(gh.rule$w * u.inverse(ms.total.gh(zs = gh.rule$x, ...), a = a))
}

