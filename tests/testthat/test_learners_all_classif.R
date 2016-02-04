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
    classif.gbm = list(bag.fraction = 1, n.minobsinnode = 1),
    classif.lssvm = list(kernel = "rbfdot", reduced = FALSE),
    classif.nodeHarvest = list(nodes = 100L, nodesize = 5L),
    classif.xyf = list(ydim = 2L)
  )

  fixHyperPars = function(lrn) {
    if (lrn$id %in% names(hyperpars))
      lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])
    return(lrn)
  }

  # binary classif
  task = subsetTask(binaryclass.task, subset = c(10:20, 180:190),
    features = getTaskFeatureNames(binaryclass.task)[12:15])
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns) {
    expect_output(print(lrn), lrn$id)
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # binary classif with factors
  data = binaryclass.df[c(10:20, 180:190), 12:15]
  data[, 4L] = factor(sample(c("a", "b"), size = nrow(data), replace = TRUE))
  data$y = binaryclass.df[c(10:20, 180:190),binaryclass.target]
  task = makeClassifTask(data = data, target = "y")
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns) {
    expect_output(print(lrn), lrn$id)
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # binary classif with prob
  task = subsetTask(binaryclass.task, subset = c(1:10, 180:190),
    features = getTaskFeatureNames(binaryclass.task)[12:15])
  lrns = mylist(task, properties = "prob")
  lrns = lapply(lrns$class, makeLearner, predict.type = "prob")
  lapply(lrns, function(lrn) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    getPredictionProbabilities(p)
    expect_true(!is.na(performance(p)))
  })

  # binary classif with weights
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  task = subsetTask(task, subset = c(1:10, 150:160), features = getTaskFeatureNames(task)[1:2])
  lrns = mylist(task, properties = "weights")
  lrns = lapply(lrns$class, makeLearner)
  lapply(lrns, function(lrn) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task, weights = 1:getTaskSize(task))
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  })

  # classif with missing
  d = binaryclass.df[c(1:10, 180:190), c(1:2, binaryclass.class.col)]
  d[1, 1] = NA
  task = makeClassifTask(data = d, target = binaryclass.target)
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # classif with factors
  d = binaryclass.df[c(1:10, 181:190), c(1:2, binaryclass.class.col)]
  d[, 2] = factor(rep(c("a", "b", "b", "a"), each = 5L))
  task = makeClassifTask(data = d, target = binaryclass.target)
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }


})

test_that("weightedClassWrapper on all binary learners",  {
  pos = getTaskDescription(binaryclass.task)$positive
  f = function(lrn, w) {
    lrn1 = makeLearner(lrn)
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, binaryclass.task)
    p = predict(m, binaryclass.task)
    cm = getConfMatrix(p)
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
    cm = getConfMatrix(p)
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

