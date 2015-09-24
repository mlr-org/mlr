context("Learner")

test_that("Learner", {
  wl = makeLearner("classif.rpart", minsplit = 3)
  expect_equal(wl$type, "classif")
  expect_equal(wl$id, "classif.rpart")
  expect_true(is.character(wl$properties))
  expect_true(length(wl$properties) >= 6)

  wl = makeLearner("regr.lm")
  expect_equal(wl$type, "regr")
  expect_equal(wl$id, "regr.lm")
  expect_true(is.character(wl$properties))

  expect_error(makeLearner("classif.lvq1", predict.type = "prob"), "Trying to predict probs, but")
  expect_error(makeLearner("regr.lm", predict.type = "prob"), "'predict.type'")
  wl = makeLearner("classif.lvq1")
  expect_error(setPredictType(wl, "prob"), "Trying to predict probs, but")

  wl = makeLearner("regr.lm", config = list(on.learner.error = "quiet"))
  expect_equal(wl$config$on.learner.error, "quiet")

  expect_error(makeLearner("classif.lda", predict.threshold = 1, "'prob' must hold"))

  #  Learner cannot use expression in param requires, see #369
  expect_error(makeLearner("classif.__mlrmocklearners__5"), "used 'expression'")
})

