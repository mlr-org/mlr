# Helper functions for testing learners specific to the type of learning task
# args: learner, task, hyperpars (list which is set up in learners_all when we
# need to deviate from the defaults for stability)


# test that a given learner respects its weights tag. we do this:
# train without weights, with weights = 1, and with changed weights
# then we check that changed weights actually change the output
# note: it is not always easy to find a good data and weights combo to reliably achieve this,
# as for some learners we need very extreme weights and for others exteme weights cause error.
#
# further args: train and test.inds, and weight vec for train inds
# we can also set pred.type and the getter for the output col from the preds.

testThatLearnerRespectsWeights = function(lrn, task, train.inds, test.inds, weights, hyperpars,
  pred.type, get.pred.fun) {

  lrn = setPredictType(lrn, pred.type)

  if (lrn$id %in% names(hyperpars))
    lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])

  rin = makeResampleInstance("Holdout", task = task)
  m1 = train(lrn, task, subset = train.inds)
  w.allone = rep(1, length(train.inds))
  m2 = train(lrn, task, subset = train.inds, weights = w.allone)
  m3 = train(lrn, task, subset = train.inds, weights = weights)
  p1 = predict(m1, task, subset = test.inds)
  p2 = predict(m2, task, subset = test.inds)
  p3 = predict(m3, task, subset = test.inds)
  perf1 = performance(p1)
  perf2 = performance(p2)
  perf3 = performance(p3)
  expect_true(!is.na(perf1), info = lrn$id)
  expect_true(!is.na(perf2), info = lrn$id)
  expect_true(!is.na(perf3), info = lrn$id)
  expect_equal(get.pred.fun(p1), get.pred.fun(p2), info = lrn$id, tolerance = 0.0001, scale = 1)
  expect_false(isTRUE(all.equal(get.pred.fun(p1), get.pred.fun(p3))), info = lrn$id)
}


# Test that learner produces output on the console, its ParamSet can be printed,
# can be trained, can predict and that a performance measure is calculated.
# This function is being used to test learners in general and in the other
# helper functions testing learners that claim to handle missings, factors,...
# It also tests if the learner can predict probabilities or standard errors.
# When testing probabilities an additional test if there are missing prediction
# probabilities and if there as many probability predictions as there are
# observations in the task.
# When testing standard errors an additional test if there are as many predictions
# as there are observations in the task is being performed.
# Note: performance() needs the task argument so that it works with cluster learners.
# further args: pred.type (only needs specification "prob" when testing learner
# can predict probabilities or specification "se" when testing learner can
# predict standard errors.)

testBasicLearnerProperties = function(lrn, task, hyperpars, pred.type = "response") {
  # handling special par.vals and predict type
  info = lrn$id
  if (lrn$id %in% names(hyperpars))
    lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])

  lrn = setPredictType(lrn, pred.type)

  # check that learner prints
  expect_output(info = info, print(lrn), lrn$id)

  # check that param set prints
  par.set = getParamSet(lrn)
  expect_output(info = info, print(par.set))

  # check that learner trains, predicts
  m = train(lrn, task)
  p = predict(m, task)
  expect_true(info = info, !is.na(performance(pred = p, task = task)))

  # check that se works and is > 0
  if (pred.type == "se") {
    s = p$data$se
    expect_numeric(info = info, s, lower = 0, finite = TRUE, any.missing = FALSE, len = getTaskSize(task))
  }
  # check that quantile works and is > 0
  if (pred.type == "quantile") {
    quantiles = p$data[, -1, drop = FALSE]
    for (i in seq_len(ncol(quantiles))) {
      expect_numeric(info = info, quantiles[, i], lower = Inf, finite = TRUE,
        any.missing = FALSE, len = getTaskSize(task))
    }
  }

  # check that probs works, and are in [0,1] and sum to 1
  if (pred.type == "prob") {
    if (inherits(lrn, "RLearnerCluster")) {
      # for unsupervised tasks we don't have any class labels
      probdf = getPredictionProbabilities(p)
      cls = colnames(probdf)
    } else {
      cls = getTaskClassLevels(task)
      probdf = getPredictionProbabilities(p, cl = cls)
    }
    expect_named(probdf, cls)
    expect_data_frame(info = info, probdf, nrows = getTaskSize(task), ncols = length(cls),
      types = "numeric", any.missing = FALSE)
    expect_true(info = info, all(probdf >= 0 && probdf <= 1))

    expect_equal(info = info, unname(rowSums(probdf)), rep(1, NROW(probdf)), use.names = FALSE, tolerance = 0.01)
  }
}


# Test that a given learner can handle factors:
# Data of the task is being manipulated so that the first feature in the data
# is a factor. A new task is being generated based on the manipulated data
# with changeData().
# Then testThatLearnerCanTrainPredict() is being called to check whether learner
# can be trained, can predict and produces reasonable performance output.

testThatLearnerHandlesFactors = function(lrn, task, hyperpars) {

  d = getTaskData(task)
  f = getTaskFeatureNames(task)[1]
  d[, f] = as.factor(rep_len(c("a", "b"), length.out = nrow(d)))
  new.task = changeData(task = task, data = d)

  testBasicLearnerProperties(lrn = lrn, task = task, hyperpars = hyperpars)
}


# Tests that learner handles ordered factors
# Data of task is manipulated such that the first mentioned feature is changed
# to a ordered factor with a < b < c.
# A new task is being generated based on the manipulated data with changeData().
# Then testThatLearnerCanTrainPredict() is being called to check whether learner
# can be trained, can predict and produces reasonable performance output.

testThatLearnerHandlesOrderedFactors = function(lrn, task, hyperpars) {

  d = getTaskData(task)
  f = getTaskFeatureNames(task)[1]
  d[, f] = as.ordered(rep_len(c("a", "b", "c"), length.out = nrow(d)))
  new.task = changeData(task = task, data = d)

  testBasicLearnerProperties(lrn = lrn, task = task, hyperpars = hyperpars)

}


# Test that a given learner can handle missings:
# Data of the task is being manipulated so that the first obervation of the first
# feature in the data is missing.
# A new task is being generated based on the manipulated data with changeData().
# Then testThatLearnerCanTrainPredict is being called to check whether learner
# can be trained, can predict and produces reasonable performance output.

testThatLearnerHandlesMissings = function(lrn, task, hyperpars) {

  d = getTaskData(task)
  f = getTaskFeatureNames(task)[1]
  d[1, f] = NA
  new.task = changeData(task = task, data = d)

  testBasicLearnerProperties(lrn = lrn, task = task, hyperpars = hyperpars)
}

# Test that the extraction of the out-of-bag predictions for the learner that supports
# this works correctly

testThatGetOOBPredsWorks = function(lrn, task) {
  type = lrn$type
  mod = train(lrn, task)
  oob = getOOBPreds(mod, task)

  if (type == "classif") {
    if (lrn$predict.type == "response") {
      expect_is(oob$data, "data.frame")
      expect_equal(levels(oob$data$response), task$task.desc$class.levels)
    } else {
      expect_is(oob$data, "data.frame")
      expect_numeric(getPredictionProbabilities(oob))
    }
  } else {
    if (type %in% c("regr", "surv")) {
      expect_is(oob$data$response, "numeric")
    }
  }
  expect_equal(nrow(oob$data), nrow(getTaskData(task)))
}

testThatLearnerCanCalculateImportance = function(lrn, task, hyperpars) {


  if (lrn$id %in% names(hyperpars))
    lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])

  # some learners need special param settings to compute variable importance
  # add them here if you implement a measure that requires that.
  # you may also want to change the params for the learner if training takes
  # a long time
  if (lrn$short.name == "ranger")
    lrn = setHyperPars(lrn, importance = "permutation")
  if (lrn$short.name == "adabag")
    lrn = setHyperPars(lrn, mfinal = 5L)
  if (lrn$short.name == "cforest")
    lrn = setHyperPars(lrn, ntree = 5L)
  if (lrn$short.name == "rfsrc")
    lrn = setHyperPars(lrn, ntree = 5L)
  if (lrn$short.name == "xgboost")
    lrn = setHyperPars(lrn, nrounds = 10L)

  mod = train(lrn, task)
  feat.imp = getFeatureImportance(mod)$res
  expect_data_frame(feat.imp, types = rep("numeric", getTaskNFeats(task)),
    any.missing = FALSE, nrows = 1, ncols = getTaskNFeats(task))
  expect_equal(colnames(feat.imp), mod$features)

}


testThatLearnerParamDefaultsAreInParamSet = function(lrn) {
  pars = lrn$par.set$pars
  pv = lrn$par.vals
  expect_true(isSubset(names(pv), names(pars)))
}
