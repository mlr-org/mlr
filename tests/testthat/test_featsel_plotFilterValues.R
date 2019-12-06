context("featsel")

test_that("n.show arg has an effect in plotFilterValues()", {
  fv = generateFilterValuesData(iris.task, method = "variance")
  fv_plot = plotFilterValues(fv, n.show = 2)

  vdiffr::expect_doppelganger("n.show", fv_plot)
})

test_that("feat.type.cols arg has an effect in plotFilterValues()", {
  fv = generateFilterValuesData(bh.task, method = "praznik_CMIM")
  fv_plot1 = plotFilterValues(fv, feat.type.cols = TRUE)

  vdiffr::expect_doppelganger("feat.type.cols", fv_plot1)
})

test_that("plotFilterValues shows the correct count when n.show > nfeat", {
  fv = generateFilterValuesData(bh.task, method = "praznik_CMIM")
  fv_plot2 = plotFilterValues(fv, n.show = 25)

  vdiffr::expect_doppelganger("n.show > nfeat", fv_plot2)
})
