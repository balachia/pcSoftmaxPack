test_that("ms.total works, vectorized", {
    # v0 = 0
    ms0.lt <- 0.213299863100135
    ms0.md <- 0.215291429008823
    ms0.rt <- 0.512814664146088
    ms0.sum <- 0.941405956255046
    # v0 = 1
    ms1.lt <- 0.512814664146088
    ms1.md <- 0.423047381883792
    ms1.rt <- 1.08202996459933
    ms1.sum <- 2.01789201062921
    # pre-run
    ms0.result <- ms.total(v0s = 0, x0 = 1, vgrid = rep(0, 2), xgrid = 0:1)
    ms01.result <- ms.total(v0s = 0:1, x0 = 1, vgrid = rep(0, 2), xgrid = 0:1)
    ms.noorder.result <- ms.total(0:1, 1, rep(0, 2), 0:1)
    # test that values are all good
    expect_equal(ms0.result$mss,
        array(c(ms0.lt, ms0.md, 0, ms0.rt), dim = c(1,4)))
    expect_equal(ms01.result$mss,
        array(c(ms0.lt, ms1.lt, ms0.md, ms1.md, 0, 0, ms0.rt, ms1.rt), dim = c(2,4)))
    expect_equal(ms0.result$ms, ms0.sum)
    expect_equal(ms01.result$ms, c(ms0.sum, ms1.sum))
    expect_equal(ms.noorder.result$mss,
        array(c(ms0.lt, ms1.lt, ms0.md, ms1.md, 0, 0, ms0.rt, ms1.rt), dim = c(2,4)))
    # try errors
    expect_error(ms.total(v0s = 0, x0 = 0:1, vgrid = 0, xgrid = 0))
    expect_error(ms.total(v0s = 0, x0 = 0, vgrid = 0:1, xgrid = 0))
    expect_error(ms.total(v0s = 0, x0 = 0, vgrid = 0, xgrid = 0:1))
})

