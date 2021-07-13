test_that("terminal values, k, v, vectorized", {
    expect_equal(mstk(c(1, expm1(1))),
        c(log(2), 1))
    expect_equal(mstv(1, numeric(0)),
        log1p(exp(1)))
    expect_equal(mstv(1, 1),
        0.5*log1p(2*exp(1)))
})

