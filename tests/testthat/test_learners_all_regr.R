context("learners_all_regr")

test_that("learners work: regr ", {

  # settings to make learners faster and deal with small data size
  hyperpars = list(
    regr.km = list(nugget = 0.01),
    regr.cforest = list(mtry = 1L, minsplit = 1, minbucket = 1),
    regr.bartMachine = list(verbose = FALSE, run_in_sample = FALSE,
      # see above
      replace_missing_data_with_x_j_bar = TRUE,
      num_iterations_after_burn_in = 10L),
    regr.nodeHarvest = list(nodes = 100L, nodesize = 5L),
    regr.h2o.deeplearning = list(hidden = 2L),
    regr.ranger = list(keep.inbag = TRUE)
  )

  # Create smaller task: dont use feature 2, it is nearly always 0, don't use
  # feature 4, it is a factor variable
  task = subsetTask(regr.task, subset = c(1:70),
    features = getTaskFeatureNames(regr.task)[c(1, 3)])

  # normal regr
  lrns = listLearnersCustom(task, create = TRUE)
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars)

  # regr with factors
  lrns = listLearnersCustom(task, properties = "factors", create = TRUE)
  lapply(lrns, testThatLearnerHandlesFactors, task = task,
    hyperpars = hyperpars)

  # regr with ordered factors
  lrns = listLearnersCustom(task, properties = "ordered", create = TRUE)
  lapply(lrns, testThatLearnerHandlesOrderedFactors, task = task,
    hyperpars = hyperpars)

  # regr with se
  lrns = listLearnersCustom(task, properties = "se", create = TRUE)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars,
    pred.type = "se")

  # regr with weights
  lrns = listLearnersCustom(task, properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = task, train.inds = 1:70, test.inds = 1:70, weights = rep(c(1, 5),
      length.out = 70),
    pred.type = "response", get.pred.fun = getPredictionResponse)

  # regr with missing
  lrns = listLearnersCustom(task, properties = "missings", create = TRUE)
  lapply(lrns, testThatLearnerHandlesMissings, task = task,
    hyperpars = hyperpars)

  # regr variable importance
  lrns = listLearnersCustom(task, properties = "featimp", create = TRUE)
  lapply(lrns, testThatLearnerCanCalculateImportance, task = task,
    hyperpars = hyperpars)

  # regr with oobpreds
  lrns = listLearnersCustom(task, properties = "oobpreds", create = TRUE)
  lapply(lrns, testThatGetOOBPredsWorks, task = task)

  # regr with only one feature
  min.task = makeRegrTask("oneCol", data.frame(x = 1:10, y = 1:10),
    target = "y")
  lrns = listLearnersCustom(min.task, create = TRUE)
  # regr.gbm: Meaningfull error about too small dataset
  # regr.cforest: Error in model@fit(data, ...) : fraction of 0.000000 is too small
  # regr.nodeHarvest: Error in ZRULES[[1]] : subscript out of bounds
  # others: see learners_all_classif and random errors
  not.working = c(
    "regr.cforest",
    "regr.cvglmnet",
    "regr.evtree",
    "regr.frbs",
    "regr.gbm",
    "regr.glmnet",
    "regr.laGP",
    "regr.nodeHarvest",
    "regr.slim")
  lrns = lrns[extractSubList(lrns, "id", simplify = TRUE) %nin% not.working]
  lapply(lrns, testBasicLearnerProperties, task = min.task,
    hyperpars = hyperpars)
})
