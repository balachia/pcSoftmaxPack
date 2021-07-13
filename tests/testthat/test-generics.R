test_that("vectorize works", {
    expect_mapequal(vectorize.args(a=1:3, b=5, c = 6),
        list(a=1:3, b=rep(5,3), c=rep(6,3)))
    expect_mapequal(vectorize.args(a=1:3, b=5, c = 6:8),
        list(a=1:3, b=rep(5,3), c=6:8))
    expect_error(vectorize.args(a=1:3, b=5, c = 6:7))
})

# TODO: test for error states?
test_that("relative.share works", {
    expect_equal(relative.share(c(1, 1 + log(2)), c(1, 1)),
        c( 1/3, 1/2 ))
})

# TODO: test for error states?
test_that("bounds.in.grid works", {
    expect_equal(bounds.in.grid(1.5, 1:5),
        list(l = 1, r = 2))
})

# TODO: test for error states?
# e.g. both endpoints missing, neither missing
test_that("open.sd works", {
    expect_equal(open.sd(1:3, xl = c(0, 0, NA), xr = c(NA, NA, 7)),
        c(1, sqrt(2), 2))
})

# TODO: test for error states?
test_that("bridge.mean works", {
    expect_equal(bridge.mean(1:3, xl = 0, xr = 4, vl = 10, vr = 12),
        c(10.5, 11, 11.5))
})

# TODO: test for error states?
test_that("bridge.sd works", {
    expect_equal(bridge.sd(1:3, xl = 0, xr = 4),
        c(sqrt(3/4), 1, sqrt(3/4)))
})
