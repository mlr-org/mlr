context("learners")

if (isExpensiveExampleOk()) {

test_that("listLearners", {
  x1 = listLearners()
  x2 = listLearners("classif")
  x3 = listLearners("regr")
  x4 = listLearners("surv")
  x5 = listLearners("cluster")
  expect_true(length(x1) > 40)
  expect_true(length(x2) > 20)
  expect_true(length(x3) > 10)
  expect_true(length(x4) > 1)
  expect_true(length(x5) > 1)
  expect_true(setequal(x1, c(x2, x3, x4, x5)))

  x5 = listLearners("classif", properties = c("multiclass", "factors", "prob"))
  expect_true(length(x5) > 10 && all(x5 %in% x2))
})

test_that("listLearners for task", {
  x1 = listLearners(binaryclass.task)
  x2 = listLearners(multiclass.task)
  x3 = listLearners(regr.task)
  expect_true(length(x1) > 10)
  expect_true(length(x2) > 10)
  expect_true(length(x3) > 10)
  expect_true(length(intersect(x1, x3)) == 0)
  expect_true(length(intersect(x2, x3)) == 0)
  expect_true(all(x2 %in% x1))
})

test_that("learners work", {
  # binary classif
  task = subsetTask(binaryclass.task, subset = c(10:50, 180:208),
    features = getTaskFeatureNames(binaryclass.task)[12:15])
  lrns = listLearners(task, create = TRUE)
  for (lrn in lrns) {
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  }

  # binary classif with prob
  task = subsetTask(binaryclass.task, subset = c(1:50, 150:208),
    features = getTaskFeatureNames(binaryclass.task)[1:2])
  lrns = listLearners(task, properties = "prob")
  lrns = lapply(lrns, makeLearner, predict.type = "prob")
  lapply(lrns, function(lrn) {
    m = train(lrn, task)
    p = predict(m, task)
    getProbabilities(p)
  })

  # binary classif with weights
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  task = subsetTask(task, subset = c(1:50, 150:208), features = getTaskFeatureNames(task)[1:2])
  lrns = listLearners(task, properties = "weights")
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    m = train(lrn, task, weights = 1:task$task.desc$size)
    p = predict(m, task)
  })

  # classif with missing
  d = binaryclass.df[c(1:50, 120:170), c(1:2, binaryclass.class.col)]
  d[1, 1] = NA
  task = makeClassifTask(data = d, target = binaryclass.target)
  lrns = listLearners(task, create = TRUE)
  lapply(lrns, function(lrn) {
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  })

  # normal regr
  task = subsetTask(regr.task, subset = c(1:70),
    features = getTaskFeatureNames(regr.task)[1:2])
  lrns = listLearners(task)
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    if (lrn$id == "regr.km")
      lrn = setHyperPars(lrn, nugget.estim = TRUE)
    m = train(lrn, task)
    p = predict(m, task)
  })

  # regr with se
  task = subsetTask(regr.task, subset = c(1:70),
  features = getTaskFeatureNames(regr.task)[1:2])
  lrns = listLearners(task, properties = "se")
  lrns = lapply(lrns, makeLearner, predict.type = "se")
  lapply(lrns, function(lrn) {
    if (lrn$id == "regr.km")
      lrn = setHyperPars(lrn, nugget.estim = TRUE)
    m = train(lrn, task)
    p = predict(m, task)
    expect_equal(length(p$data$se), 70)
  })

  # regr with weights
  task = subsetTask(regr.task, subset = 1:70, features = getTaskFeatureNames(regr.task)[1:2])
  lrns = listLearners(task, properties = "weights")
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    if (lrn$id == "regr.km")
      lrn = setHyperPars(lrn, nugget.estim = TRUE)
    m = train(lrn, task, weights = 1:task$task.desc$size)
    p = predict(m, task)
  })

  # regr with missing
  d = regr.df[1:100, c(getTaskFeatureNames(regr.task)[1:2], regr.target)]
  d[1, 1] = NA
  task = makeRegrTask(data = d, target = regr.target)
  lrns = listLearners(task, create = TRUE)
  lapply(lrns, function(lrn) {
    m = train(lrn, task)
    p = predict(m, task)
    expect_true(!is.na(performance(p)))
  })

})

}
