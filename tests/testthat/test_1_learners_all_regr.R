
test_that("learners work: regr ", {

  # because of missing rJava for bartMachine
  skip_on_os("windows")

  suppressPackageStartupMessages(library("crs", quietly = TRUE))
  library("kknn", quietly = TRUE)
  suppressPackageStartupMessages(library("penalized", quietly = TRUE))
  library("survival", quietly = TRUE)
  #suppressPackageStartupMessages(requirePackagesOrSkip("crs", default.method = "load"))

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
  lrns = suppressMessages(listLearnersCustom(task, create = TRUE))
  foo = lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  foo = lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars)

  # regr with factors
  lrns = suppressMessages(listLearnersCustom(task, properties = "factors", create = TRUE))
  foo = lapply(lrns, testThatLearnerHandlesFactors, task = task,
    hyperpars = hyperpars)

  # regr with ordered factors
  lrns = suppressMessages(listLearnersCustom(task, properties = "ordered", create = TRUE))
  foo = lapply(lrns, testThatLearnerHandlesOrderedFactors, task = task,
    hyperpars = hyperpars)

  # regr with se
  lrns = suppressMessages(listLearnersCustom(task, properties = "se", create = TRUE))
  foo = lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars,
    pred.type = "se")

  # regr with weights
  lrns = suppressMessages(listLearnersCustom(task, properties = "weights", create = TRUE))
  foo = lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = task, train.inds = 1:70, test.inds = 1:70, weights = rep(c(1, 5),
      length.out = 70),
    pred.type = "response", get.pred.fun = getPredictionResponse)

  # regr with missing
  lrns = suppressMessages(listLearnersCustom(task, properties = "missings", create = TRUE))
  foo = lapply(lrns, testThatLearnerHandlesMissings, task = task,
    hyperpars = hyperpars)

  # regr variable importance
  lrns = suppressMessages(listLearnersCustom(task, properties = "featimp", create = TRUE))
  foo = lapply(lrns, testThatLearnerCanCalculateImportance, task = task,
    hyperpars = hyperpars)

  # regr with oobpreds
  lrns = suppressMessages(listLearnersCustom(task, properties = "oobpreds", create = TRUE))
  foo = lapply(lrns, testThatGetOOBPredsWorks, task = task)

  # regr with only one feature
  min.task = makeRegrTask("oneCol", data.frame(x = 1:10, y = 1:10),
    target = "y")
  lrns = suppressMessages(listLearnersCustom(min.task, create = TRUE))
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
  foo = suppressWarnings(lapply(lrns, testBasicLearnerProperties, task = min.task,
    hyperpars = hyperpars))
})
