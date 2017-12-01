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

  # Create smaller task: dont use feature 2, it is nearly always 0, don't use feature 4, it is a factor variable
  task = subsetTask(regr.task, subset = c(1:70), features = getTaskFeatureNames(regr.task)[c(1, 3)])

  # normal regr
  lrns = mylist(task, create = TRUE)
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars)

  # regr with factors
  lrns = mylist(task, properties = "factors", create = TRUE)
  lapply(lrns, testThatLearnerHandlesFactors, task = task, hyperpars = hyperpars)

  # regr with ordered factors
  lrns = mylist(task, properties = "ordered", create = TRUE)
  lapply(lrns, testThatLearnerHandlesOrderedFactors, task = task, hyperpars = hyperpars)

  # regr with se
  lrns = mylist(task, properties = "se", create = TRUE)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars,
    pred.type = "se")

  # regr with weights
  lrns = mylist(task, properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = task, train.inds = 1:70, test.inds = 1:70, weights = rep(c(1, 5), length.out = 70),
    pred.type = "response", get.pred.fun = getPredictionResponse)

  # regr with missing
  lrns = mylist(task, properties = "missings", create = TRUE)
  lapply(lrns, testThatLearnerHandlesMissings, task = task, hyperpars = hyperpars)

  # regr variable importance
  lrns = mylist(task, properties = "featimp", create = TRUE)
  lapply(lrns, testThatLearnerCanCalculateImportance, task = task, hyperpars = hyperpars)

  # regr with oobpreds
  lrns = mylist(task, properties = "oobpreds", create = TRUE)
  lapply(lrns, testThatGetOOBPredsWorks, task = task)
})
