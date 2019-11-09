context("getHyperPars")

test_that("getHyperPars", {
  lrn = makeLearner("classif.rpart")
  expect_equal(getHyperPars(lrn), list(xval = 0))

  lrn = makeLearner("classif.lda")
  named.list = list()
  names(named.list) = character(0)
  expect_equal(getHyperPars(lrn), named.list)

  lrn = makeFilterWrapper(makeLearner("classif.rpart"))
  expect_true(setequal(names(getHyperPars(lrn)), c("xval", "fw.method")))

  lrn = makeModelMultiplexer(list("classif.rpart", "classif.lda"))
  expect_true(setequal(names(getHyperPars(lrn)), c("classif.rpart.xval", "selected.learner")))

  lrn = makeLearner("multilabel.rFerns")
  expect_true(setequal(getHyperPars(lrn), list()))

  lrn = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
  expect_true(setequal(getHyperPars(lrn), list(xval = 0)))

  # Missing values should not be omitted and printed
  lrn = makeLearner("classif.xgboost", missing = NA)
  expect_output(print(lrn), "missing=NA")
  lrn = makeLearner("regr.xgboost", missing = NA)
  expect_output(print(lrn), "missing=NA")
})
