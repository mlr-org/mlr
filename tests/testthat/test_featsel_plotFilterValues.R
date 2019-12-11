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

test_that("plotFilterValues errors if arg 'filter' is not avail", {
  fv = generateFilterValuesData(bh.task, method = "praznik_CMIM")
  expect_error(plotFilterValues(fv, filter = "foo"))
})

test_that("plotFilterValues errors if arg 'filter' is not avail", {
  fv = generateFilterValuesData(iris.task, method = c("variance", "praznik_CMIM"))
  expect_error(plotFilterValues(fv))
})

test_that("plotFilterValues arg 'filter' works", {
  fv = generateFilterValuesData(iris.task, method = c("variance", "praznik_CMIM"))
  fv_plot3 = plotFilterValues(fv, filter = "variance")

  vdiffr::expect_doppelganger("filter argument", fv_plot3)
})
