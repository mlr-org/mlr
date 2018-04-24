context("configureMlr")

test_that("getOptions and configureMlr", {
  mlr.options = getMlrOptions()
  expect_equal(length(mlr.options), 8L)

  configureMlr(on.learner.error = "quiet")
  expect_equal(getMlrOptions()$on.learner.error, "quiet")

  do.call(configureMlr, mlr.options)
  expect_equal(getMlrOptions(), mlr.options)
})
