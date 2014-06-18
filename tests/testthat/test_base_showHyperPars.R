context("showHyperPars")

test_that("showHyperPars", {
  lrns = listLearners()
  capture.output({
  for (lrn in lrns) {
    showHyperPars(lrn)
  }
  })
})
