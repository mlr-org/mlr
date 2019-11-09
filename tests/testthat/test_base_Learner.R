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
})

test_that("Learner operators work", {
  lrn = makeLearner("classif.lda", nu = 3)
  expect_equal(getLearnerType(lrn), "classif")
  expect_equal(getLearnerId(lrn), "classif.lda")
  expect_equal(getLearnerPredictType(lrn), "response")
  expect_equal(getLearnerPackages(lrn), "MASS")

  # getLearnerParVals is simply a synonym for getHyperPars and that has its own test file
  expect_equal(getLearnerParVals(lrn), list(nu = 3))
  # getLearnerParamSet is simply a synonym for getParamSet, which is also tested elsewhere
  expect_equal(getLearnerParamSet(lrn), getParamSet(lrn))
  lrn2 = setLearnerId(lrn, "foo")
  expect_equal(getLearnerId(lrn2), "foo")

  # test that packages get combines
  lrn2 = makeFilterWrapper(lrn, fw.method = "randomForest_importance")
  expect_set_equal(getLearnerPackages(lrn2), c("MASS", "randomForest"))

  # test getLearnerShortName
  expect_equal(getLearnerShortName(lrn), lrn$short.name)
  wrapped.lrn = makeBaggingWrapper(lrn)
  wrapped.lrn.short.name = paste(c(lrn$short.name, "bagged"), collapse = ".")
  expect_equal(getLearnerShortName(wrapped.lrn), wrapped.lrn.short.name)
  wrapped.lrn = makeImputeWrapper(wrapped.lrn)
  wrapped.lrn.short.name = paste(c(wrapped.lrn.short.name, "imputed"), collapse = ".")
  expect_equal(getLearnerShortName(wrapped.lrn), wrapped.lrn.short.name)
})
