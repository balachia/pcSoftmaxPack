test_that("msik atan argument builder works, vectorized", {
    expect_equal(arg.atanx(1, 1, c(log(2), log(3))),
        c(1/9, 2/12))
    expect_equal(arg.atanx(2, 1, c(log(2), log(3))),
        c(1/11, 2/14))
    expect_equal(arg.atanx(c(1,2), 1, c(log(2), log(3))),
        c(1/9, 2/14))
})

test_that("msik works, vectorized", {
    # verified against mathematica
    expect_equal(msik.baseline(1, 1, c(1, 5)),
        c(0.194970566488315, 0.246983652294815))
    expect_equal(msik.baseline(c(1,5), c(1,5), c(1, 5)),
        c(0.194970566488315, 0.320304117985577))
    expect_equal(msik(c(1,5), c(1,5), c(1, 5)),
        c(0.194970566488315, 0.320304117985577))
    # test tolerances
    mathematica <- 9.35719813341421e-14
    r.double <- msik.baseline(exp(-30), exp(-40), 10) # should be off by 'a fair bit'
    r.mpfr <- msik(exp(-30), exp(-40), 10)
    expect_gt(abs(r.double/mathematica) - 1,
        0.2)
    expect_equal(abs(r.mpfr/mathematica) - 1,
        0)
})

test_that("msiv works, vectorized", {
    # verified against mathematica
    expect_equal(msiv(v0s = 0, vds = numeric(0), vis = 0, d = c(1, 5)),
        c(0.194970566488315, 0.246983652294815))
    expect_equal(msiv(v0s = 0:1, vds = 0, vis = 0, d = 5),
        c(0.209967058239244, 0.462076693291731))
})
