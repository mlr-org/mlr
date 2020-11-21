
test_that("learners work: classif", {

  # because of missing rJava for bartMachine
  skip_on_os("windows")

  # settings to make learners faster and deal with small data size
  hyperpars = list(
    classif.boosting = list(mfinal = 2L),
    classif.cforest = list(mtry = 1L, minsplit = 1, minbucket = 1),
    classif.bdk = list(ydim = 2L),
    classif.earth = list(degree = 3L, nprune = 2L),
    classif.gbm = list(bag.fraction = 1, n.minobsinnode = 1),
    classif.lssvm = list(kernel = "rbfdot", reduced = FALSE),
    classif.nodeHarvest = list(nodes = 100L, nodesize = 5L),
    classif.xyf = list(ydim = 2L),
    classif.h2o.deeplearning = list(hidden = 2L),
    classif.FDboost = list(mstop = 2L)
  )

  # binary classif
  task = subsetTask(binaryclass.task,
    subset = c(10:20, 180:190),
    features = getTaskFeatureNames(binaryclass.task)[12:15])
  lrns = suppressMessages(listLearnersCustom(task, create = TRUE))
  # some learners are not avail on windows
  if (Sys.info()[["sysname"]] == "Windows") {
    names = vapply(lrns, function(x) x$id, FUN.VALUE = character(1))
    row_ids = which(names %in% "classif.IBk")
    lrns[row_ids] = NULL
  }

  foo = lapply(lrns, testThatLearnerParamDefaultsAreInParamSet)
  foo = suppressWarnings(lapply(lrns, testBasicLearnerProperties, task = task, hyperpars = hyperpars))

  # binary classif with factors
  lrns = suppressMessages(listLearnersCustom(task, properties = "factors", create = TRUE))

  foo = capture.output(suppressWarnings(lapply(lrns, testThatLearnerHandlesFactors,
    task = task,
    hyperpars = hyperpars)))

  # binary classif with ordered factors
  lrns = listLearnersCustom(task, properties = "ordered", create = TRUE)

  foo = lapply(lrns, testThatLearnerHandlesOrderedFactors,
    task = task,
    hyperpars = hyperpars)

  # binary classif with prob
  lrns = listLearnersCustom(binaryclass.task, properties = "prob", create = TRUE)
  foo = suppressWarnings(lapply(lrns, testBasicLearnerProperties,
    task = binaryclass.task,
    hyperpars = hyperpars, pred.type = "prob"))

  # binary classif with weights
  lrns = listLearnersCustom(binaryclass.task, properties = "weights", create = TRUE)
  foo = suppressWarnings(lapply(lrns, testThatLearnerRespectsWeights,
    hyperpars = hyperpars,
    task = binaryclass.task, train.inds = binaryclass.train.inds,
    test.inds = binaryclass.test.inds,
    weights = rep(c(10000L, 1L), c(10L, length(binaryclass.train.inds) - 10L)),
    pred.type = "prob", get.pred.fun = getPredictionProbabilities))

  # classif with missing
  lrns = listLearnersCustom(task, properties = "missings", create = TRUE)
  foo = suppressWarnings(lapply(lrns, testThatLearnerHandlesMissings, task = task, hyperpars = hyperpars))

  # classif with oobpreds
  # FIXME: rFerns issue: https://notabug.org/mbq/rFerns/issues/3
  lrns = listLearnersCustom(task, properties = "oobpreds", create = TRUE)
  names = vapply(lrns, function(x) x$id, FUN.VALUE = character(1))
  row_ids = which(names %in% "classif.rFerns")
  lrns[row_ids] = NULL

  foo = lapply(lrns, testThatGetOOBPredsWorks, task = task)

  # classif with oobpreds and probability
  lrns = listLearnersCustom(task, properties = c("oobpreds", "prob"), create = TRUE)
  lrns = lapply(lrns, setPredictType, predict.type = "prob")
  foo = lapply(lrns, testThatGetOOBPredsWorks, task = task)

  # classif with variable importance
  lrns = listLearnersCustom(task, properties = "featimp", create = TRUE)
  foo = lapply(lrns, testThatLearnerCanCalculateImportance,
    task = task,
    hyperpars = hyperpars)

  # classif with only one feature
  min.task = makeClassifTask("oneCol", data.frame(
    x = 1:10,
    y = as.factor(rep(c("a", "b"), each = 5))), target = "y")
  lrns = listLearnersCustom(min.task, create = TRUE)
  # FIXME: classif.boosting: Remove if bug is removed in adabag!
  # FIXME: classif.quaDA: Remove if bug is removed in DiscriMiner::quaDA!
  # FIXME: classif.rknn: Remove if bug is removed in rknn::rknn!
  # classif.cvglmnet does not claim to work for 1d problems
  # classif.dbnDNN, classif.evtree, classif.geoDA, classif.linDA, classif.lqa
  # (not im mlr anymore),
  # classif.lvq1, classif.mda (maybe only subset error),
  # classif.pamr (maybe only subset error),
  # classif.plsdaCaret (error maybe fixable in caret),
  # classif.rotationForest (gives some error, no one would use it for 1d anyway),
  # classif.rrlda error eccours in the learner.
  # classif.cforest: fraction of 0.000000 is too small (only travis?)
  not.working = c(
    "classif.boosting",
    "classif.cforest",
    "classif.cvglmnet",
    "classif.dbnDNN",
    "classif.evtree",
    "classif.geoDA",
    "classif.glmnet",
    "classif.linDA",
    "classif.lvq1",
    "classif.mda",
    "classif.pamr",
    "classif.plsdaCaret",
    "classif.quaDA",
    "classif.rotationForest",
    "classif.rrlda",
    "classif.rknn")
  lrns_sub = lrns[extractSubList(lrns, "id", simplify = TRUE) %nin% not.working]
  foo = suppressWarnings(lapply(lrns_sub, testBasicLearnerProperties,
    task = min.task,
    hyperpars = hyperpars))
})


test_that("weightedClassWrapper on all binary learners", {
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


test_that("WeightedClassWrapper on all multiclass learners", {
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
