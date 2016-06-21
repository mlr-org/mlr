context("learners_all_regr")

test_that("learners work: regr ", {

  # settings to make learners faster and deal with small data size
  hyperpars = list(
    regr.km = list(nugget = 0.01),
    regr.cforest = list(mtry = 1L),
    regr.bartMachine = list(verbose = FALSE, run_in_sample = FALSE,
      # see above
      replace_missing_data_with_x_j_bar = TRUE,
      num_iterations_after_burn_in = 10L),
    regr.nodeHarvest = list(nodes = 100L, nodesize = 5L),
    regr.h2o.deeplearning = list(hidden = 2L, seed = getOption("mlr.debug.seed"), reproducible = TRUE),
    regr.h2o.randomForest = list(seed = getOption("mlr.debug.seed"))
  )

  fixHyperPars = function(lrn) {
    if (lrn$id %in% names(hyperpars))
      lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])
    return(lrn)
  }

  # normal regr, dont use feature 2, it is nearly always 0
  task = subsetTask(regr.task, subset = c(1:70),
    features = getTaskFeatureNames(regr.task)[c(1, 3)])
  lrns = mylist(task)
  lrns = lapply(lrns$class, makeLearner)
  for (lrn in lrns) {
    expect_output(print(lrn), lrn$id)
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # regr with factors
  task = subsetTask(regr.task, subset = 180:240, features = getTaskFeatureNames(regr.task)[c(1, 2)])
  lrns = mylist(task)
  lrns = lapply(lrns$class, makeLearner)
  for (lrn in lrns) {
    expect_output(print(lrn), lrn$id)
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # regr with se
  task = subsetTask(regr.task, subset = c(1:70),
  features = getTaskFeatureNames(regr.task)[c(1, 3)])
  lrns = mylist(task, properties = "se")
  lrns = lapply(lrns$class, makeLearner, predict.type = "se")
  for (lrn in lrns) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_equal(length(p$data$se), 70)
    expect_true(!is.na(performance(p)))
  }

  # regr with weights
  task = subsetTask(regr.task, subset = 1:70, features = getTaskFeatureNames(regr.task)[c(1, 3)])
  lrns = mylist("regr", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = task, train.inds = 1:70, test.inds = 1:70, weights = rep(c(1, 5), length.out = 70),
    pred.type = "response", get.pred.fun = getPredictionResponse)

  # regr with missing
  d = regr.df[1:100, c(getTaskFeatureNames(regr.task)[c(1, 3)], regr.target)]
  d[1, 1] = NA
  task = makeRegrTask(data = d, target = regr.target)
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns$class) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

})
