context("learners_all_classif")

test_that("learners work: classif ", {

  # settings to make learners faster and deal with small data size
  hyperpars = list(
    classif.boosting = list(mfinal = 2L),
    classif.cforest = list(mtry = 1L),
    classif.bartMachine = list(verbose = FALSE, run_in_sample = FALSE,
      # without this (and despite use_missing_data being TRUE), the test with missing data fails with a null point exception, which manifests itself as a completely different rJava error in the test
      replace_missing_data_with_x_j_bar = TRUE,
      num_iterations_after_burn_in = 10L),
    classif.bdk = list(ydim = 2L),
    classif.earth = list(degree = 3L, nprune = 2L),
    classif.gbm = list(bag.fraction = 1, n.minobsinnode = 1),
    classif.lssvm = list(kernel = "rbfdot", reduced = FALSE),
    classif.nodeHarvest = list(nodes = 100L, nodesize = 5L),
    classif.xyf = list(ydim = 2L),
    classif.h2o.deeplearning = list(hidden = 2L, seed = getOption("mlr.debug.seed"), reproducible = TRUE),
    classif.h2o.randomForest = list(seed = getOption("mlr.debug.seed"))
  )

  # binary classif
  task = subsetTask(binaryclass.task, subset = c(10:20, 180:190),
    features = getTaskFeatureNames(binaryclass.task)[12:15])
  lrns = mylist(task, create = TRUE)
  lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars)

  # binary classif with factors
  lrns = mylist("classif", properties = "factors", create = TRUE)
  lapply(lrns, testThatLearnerHandlesFactors, task = task, hyperpars = hyperpars)

  # binary classif with ordered factors
  lrns = mylist("classif", properties = "ordered", create = TRUE)
  lapply(lrns, testThatLearnerHandlesOrderedFactors, task = task, hyperpars = hyperpars)

  # binary classif with prob
  lrns = mylist(binaryclass.task, properties = "prob", create = TRUE)
  lapply(lrns, testBasicLearnerProperties, task = binaryclass.task,
    hyperpars = hyperpars, pred.type = "prob")

  # binary classif with weights
  lrns = mylist("classif", properties = "weights", create = TRUE)
  lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
    task = binaryclass.task, train.inds = binaryclass.train.inds, test.inds = binaryclass.test.inds,
    weights = rep(c(10000L, 1L), c(10L, length(binaryclass.train.inds) - 10L)),
    pred.type = "prob", get.pred.fun = getPredictionProbabilities)

  # classif with missing
  lrns = mylist("classif", properties = "missings", create = TRUE)
  lapply(lrns, testThatLearnerHandlesMissings, task = task, hyperpars = hyperpars)

  # classif with oobpreds
  lrns = mylist("classif", properties = "oobpreds", create = TRUE)
  lapply(lrns, testThatGetOOBPredsWorks, task = task)
  # classif with oobpreds and probability
  lrns = mylist("classif", properties = c("oobpreds", "prob"), create = TRUE)
  lrns = lapply(lrns, setPredictType, predict.type = "prob")
  lapply(lrns, testThatGetOOBPredsWorks, task = task)

  # classif with variable importance
  lrns = mylist("classif", properties = "featimp", create = TRUE)
  lapply(lrns, testThatLearnerCanCalculateImportance, task = task, hyperpars = hyperpars)
})


test_that("weightedClassWrapper on all binary learners",  {
  pos = getTaskDesc(binaryclass.task)$positive
  f = function(lrn, w) {
    lrn1 = makeLearner(lrn)
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, binaryclass.task)
    p = predict(m, binaryclass.task)
    cm = calculateConfusionMatrix(p)$result
  }

  learners = listLearners(binaryclass.task, "class.weights")
  x = lapply(learners$class, function(lrn) {
    cm1 = f(lrn, 0.001)
    cm2 = f(lrn, 1)
    cm3 = f(lrn, 1000)
    expect_true(all(cm1[, pos] <= cm2[, pos]))
    expect_true(all(cm2[, pos] <= cm3[, pos]))
  })
})


test_that("WeightedClassWrapper on all multiclass learners",  {
  levs = getTaskClassLevels(multiclass.task)
  f = function(lrn, w) {
    lrn1 = makeLearner(lrn)
    param = lrn1$class.weights.param
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, multiclass.task)
    p = predict(m, multiclass.task)
    cm = calculateConfusionMatrix(p)$result
  }

  learners = listLearners(multiclass.task, "class.weights")
  x = lapply(learners$class, function(lrn) {
    classes = getTaskFactorLevels(multiclass.task)[[multiclass.target]]
    n = length(classes)
    cm1 = f(lrn, setNames(object = c(10000, 1, 1), classes))
    cm2 = f(lrn, setNames(object = c(1, 10000, 1), classes))
    cm3 = f(lrn, setNames(object = c(1, 1, 10000), classes))
    expect_true(all(cm1[, levs[1]] >= cm2[, levs[1]]))
    expect_true(all(cm1[, levs[1]] >= cm3[, levs[1]]))
    expect_true(all(cm2[, levs[2]] >= cm1[, levs[2]]))
    expect_true(all(cm2[, levs[2]] >= cm3[, levs[2]]))
    expect_true(all(cm3[, levs[3]] >= cm1[, levs[3]]))
    expect_true(all(cm3[, levs[3]] >= cm2[, levs[3]]))
  })
})
