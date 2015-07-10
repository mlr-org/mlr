context("getHyperPars")


test_that("getHyperPars", {
  lrn = makeLearner("classif.rpart")
  expect_equal(getHyperPars(lrn), list(xval = 0))
  lrn = makeLearner("classif.lda")
  expect_equal(getHyperPars(lrn), list())

  lrn = makeFilterWrapper(makeLearner("classif.rpart"))
  expect_true(setequal(names(getHyperPars(lrn)), c("xval", "fw.method")))

  lrn = makeModelMultiplexer(list("classif.rpart", "classif.lda"))
  expect_true(setequal(names(getHyperPars(lrn)), c("classif.rpart.xval", "selected.learner")))

  lrn = makeLearner("multilabel.rFerns")
  expect_true(setequal(getHyperPars(lrn), list()))

  lrn = makeMultilabelBinaryRelevanceWrapper("classif.rpart")  
  expect_true(setequal(getHyperPars(lrn), list(xval=0)))
})
