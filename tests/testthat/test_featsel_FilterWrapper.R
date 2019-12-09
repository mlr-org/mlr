context("FilterWrapper")

test_that("FilterWrapper", {
  lrn1 = makeLearner("classif.lda")
  lrn2 = makeFilterWrapper(lrn1, fw.method = "FSelectorRcpp_information.gain",
    fw.perc = 1)
  m = train(lrn2, binaryclass.task)
  expect_true(!inherits(m, "FailureModel"))
  expect_equal(m$features, getTaskFeatureNames(binaryclass.task))

  lrn2 = makeFilterWrapper(lrn1, fw.method = "FSelectorRcpp_information.gain",
    fw.abs = 0L)
  m = train(lrn2, binaryclass.task)
  expect_equal(getLeafModel(m)$features, character(0))
  expect_true(inherits(getLeafModel(m)$learner.model, "NoFeaturesModel"))

  lrn2 = makeFilterWrapper(lrn1, fw.method = "FSelectorRcpp_information.gain",
    fw.perc = 0.1)
  res = makeResampleDesc("CV", iters = 2)
  r = resample(lrn2, binaryclass.task, res)
  expect_true(!any(is.na(r$aggr)))
  expect_subset(r$extract[[1]][[1]], getTaskFeatureNames(binaryclass.task))
})

test_that("FilterWrapper univariate (issue #516)", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeFilterWrapper(lrn1, fw.method = "univariate.model.score",
    fw.perc = 1)
  m = train(lrn2, binaryclass.task)
  expect_true(!inherits(m, "FailureModel"))
  expect_equal(m$features, getTaskFeatureNames(binaryclass.task))
})

test_that("Filterwrapper permutation.importance (issue #814)", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeFilterWrapper(lrn1, fw.method = "permutation.importance",
    imp.learner = "classif.rpart", fw.perc = 1L, nmc = 1L)
  m = train(lrn2, binaryclass.task)
  res = makeResampleDesc("CV", iters = 2)
  r = resample(lrn2, binaryclass.task, res)
  expect_true(!any(is.na(r$aggr)))
  expect_subset(r$extract[[1]][[1]], getTaskFeatureNames(binaryclass.task))
})

test_that("FilterWrapper with ensemble function in a train call", {
  lrn = makeLearner("classif.lda")

  # no base.method as ensemble method if fw.base.methods !is.null
  expect_error(makeFilterWrapper(lrn, fw.method = "FSelectorRcpp_gain.ratio",
    fw.base.methods = c("FSelectorRcpp_gain.ratio",
      "FSelectorRcpp_information.gain")))

  # no ensemble methods in base.methods
  expect_error(makeFilterWrapper(lrn, fw.method = "E-min",
    fw.base.methods = c("E-min", "information.gain")))

  # multiple fw.methods are not allowed when creating a filter.wrapper ->
  # multiple inputs need to be specified in the par.set
  expect_error(makeFilterWrapper(lrn, fw.method = c("FSelector_chi.squared",
    "FSelectorRcpp_information.gain")))

  lrn2 = makeFilterWrapper(lrn, fw.method = "E-min",
    fw.base.methods = c("FSelector_chi.squared",
      "FSelectorRcpp_information.gain"),
    fw.perc = 0.43)

  # this must also work -> base.methods can be given in the par.set
  expect_silent(makeFilterWrapper(lrn, fw.method = "E-min", fw.perc = 0.43))

  m = train(lrn2, binaryclass.task)

  expect_class(m, c("FilterModel", "BaseWrapperModel", "WrappedModel"))
})
