test_that("inverse utility works", {
    expect_equal(u.inverse(2*log1p(exp(1:10)), a = 1),
        1:10)
    expect_equal(u.inverse(2*log1p(exp(1:2)), a = 0.9),
        c(0.815665301314057, 1.75470643927240))
    expect_equal(u.inverse(2*log1p(exp(1:2)), a = 0.9),
        log((1+exp(1:2))^(0.9) - 1))
})
