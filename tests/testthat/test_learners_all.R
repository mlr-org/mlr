context("learners")

mylist = function(..., create = FALSE) {
  lrns = listLearners(..., create = create)
  if (create) {
    ids = extractSubList(lrns, "id")
  } else {
    ids = lrns
  }
  lrns[!grepl("mock", ids)]
}

test_that("listLearners", {
  x1 = mylist()
  x2 = mylist("classif")
  x3 = mylist("regr")
  x4 = mylist("surv")
  x5 = mylist("cluster")
  expect_true(length(x1) > 40)
  expect_true(length(x2) > 20)
  expect_true(length(x3) > 10)
  expect_true(length(x4) > 1)
  expect_true(length(x5) > 1)
  expect_true(setequal(x1, c(x2, x3, x4, x5)))

  x6 = mylist("classif", properties = c("multiclass", "factors", "prob"))
  expect_true(length(x6) > 10 && all(x6 %in% x2))
})

test_that("listLearners for task", {
  x1 = mylist(binaryclass.task)
  x2 = mylist(multiclass.task)
  x3 = mylist(regr.task)
  expect_true(length(x1) > 10)
  expect_true(length(x2) > 10)
  expect_true(length(x3) > 10)
  expect_true(length(intersect(x1, x3)) == 0)
  expect_true(length(intersect(x2, x3)) == 0)
  expect_true(all(x2 %in% x1))
})

test_that("learners work", {

  # settings to make learnners faster and deal with small data size
  hyperpars = list(
    classif.boosting = list(mfinal = 2L),
    classif.cforest = list(mtry = 1L),
    classif.bartMachine = list(verbose = FALSE, run_in_sample = FALSE,
      num_iterations_after_burn_in = 10L),
    classif.bdk = list(ydim = 2L),
    classif.gbm = list(bag.fraction = 1, n.minobsinnode = 1),
    classif.lssvm = list(kernel = "rbfdot", reduced = FALSE),
    classif.xyf = list(ydim = 2L),
    regr.km = list(nugget = 0.01),
    regr.cforest = list(mtry = 1L),
    regr.bartMachine = list(verbose = FALSE, run_in_sample = FALSE,
      num_iterations_after_burn_in = 10L)
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
  lrns = lapply(lrns, makeLearner, predict.type = "prob")
  lapply(lrns, function(lrn) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    getProbabilities(p)
    expect_true(!is.na(performance(p)))
  })

  # binary classif with weights
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  task = subsetTask(task, subset = c(1:10, 150:160), features = getTaskFeatureNames(task)[1:2])
  lrns = mylist(task, properties = "weights")
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task, weights = 1:task$task.desc$size)
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

  # normal regr, dont use feature 2, it is nearly always 0
  task = subsetTask(regr.task, subset = c(1:70),
    features = getTaskFeatureNames(regr.task)[c(1, 3)])
  lrns = mylist(task)
  lrns = lapply(lrns, makeLearner)
  for(lrn in lrns) {
    expect_output(print(lrn), lrn$id)
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # regr with factors
  task = subsetTask(regr.task, subset = 180:240, features = getTaskFeatureNames(regr.task)[c(1, 2)])
  lrns = mylist(task)
  lrns = lapply(lrns, makeLearner)
  for(lrn in lrns) {
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
  lrns = lapply(lrns, makeLearner, predict.type = "se")
  for (lrn in lrns) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_equal(length(p$data$se), 70)
    expect_true(!is.na(performance(p)))
  }

  # regr with weights
  task = subsetTask(regr.task, subset = 1:70, features = getTaskFeatureNames(regr.task)[c(1, 3)])
  lrns = mylist(task, properties = "weights")
  lrns = lapply(lrns, makeLearner)
  for (lrn in lrns) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task, weights = rep(1:2, 35))
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # regr with missing
  d = regr.df[1:100, c(getTaskFeatureNames(regr.task)[c(1, 3)], regr.target)]
  d[1, 1] = NA
  task = makeRegrTask(data = d, target = regr.target)
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns) {
    lrn = fixHyperPars(lrn)
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # clustering, response
  task = noclass.task
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns) {
    # FIXME: remove this if DBscan runs stable
    if (!inherits(lrn, "cluster.DBScan")) {
      expect_output(print(lrn), lrn$id)
      m = train(lrn, task)
      p = predict(m, task)
      expect_true(!is.na(performance(p, task = task)))
    }
  }

  # clustering, prob
  task = subsetTask(noclass.task, subset = 1:20,
    features = getTaskFeatureNames(noclass.task)[1:2])
  lrns = mylist(task, properties = "prob")
  lrns = lapply(lrns, makeLearner, predict.type = "prob")
  lapply(lrns, function(lrn) {
    m = train(lrn, task)
    p = predict(m, task)
    getProbabilities(p)
    expect_true(!is.na(performance(p, task = task)))
  })
})
