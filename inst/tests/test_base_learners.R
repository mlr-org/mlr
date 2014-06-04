context("learners")

if (isExpensiveExampleOk()) {

test_that("listLearners", {
  x1 = listLearners()
  x2 = listLearners(type="classif")
  x3 = listLearners(type="regr")
  x4 = listLearners(type="surv")
  expect_true(length(x1) > 40)
  expect_true(length(x2) > 20)
  expect_true(length(x3) > 10)
  expect_true(length(x4) > 1)
  expect_true(setequal(x1, c(x2, x3, x4)))

  x5 = listLearners(type="classif", multiclass=TRUE, factors=TRUE, prob=TRUE)
  expect_true(length(x5) > 10 && all(x5 %in% x2))
})

test_that("listLearnersForTask", {
  x1 = listLearnersForTask(task=binaryclass.task)
  x2 = listLearnersForTask(task=multiclass.task)
  x3 = listLearnersForTask(task=regr.task)
  expect_true(length(x1) > 10)
  expect_true(length(x2) > 10)
  expect_true(length(x3) > 10)
  expect_true(length(intersect(x1, x3)) == 0)
  expect_true(length(intersect(x2, x3)) == 0)
  expect_true(all(x2 %in% x1))
})

test_that("learners work", {
  # binary classif
  task = subsetTask(binaryclass.task, subset=c(1:50, 150:208),
    features=getTaskFeatureNames(binaryclass.task)[1:2])
  lrns = listLearnersForTask(task=task)
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    # there seems to be some numerical problem with plsDA on the subsetted data...?
    task2 = if (lrn == "classif.plsDA")
      subsetTask(binaryclass.task, subset=c(1:50, 150:208),
        features=getTaskFeatureNames(binaryclass.task)[1:15])
    else
      task
    m = train(lrn, task2)
    p = predict(m, task2)
  })

  # binary classif with prob
  task = subsetTask(binaryclass.task, subset=c(1:50, 150:208),
    features=getTaskFeatureNames(binaryclass.task)[1:2])
  lrns = listLearnersForTask(task=task, prob=TRUE)
  lrns = lapply(lrns, makeLearner, predict.type="prob")
  lapply(lrns, function(lrn) {
    m = train(lrn, task)
    p = predict(m, task)
    getProbabilities(p)
  })

  # binary classif with weights
  task = makeClassifTask(data=binaryclass.df, target=binaryclass.target)
  task = subsetTask(task, subset=c(1:50, 150:208), features=getTaskFeatureNames(task)[1:2])
  lrns = listLearnersForTask(task=task, weights=TRUE)
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    m = train(lrn, task, weights=1:task$task.desc$size)
    p = predict(m, task)
  })

  # normal regr
  task = subsetTask(regr.task, subset=c(1:70),
    features=getTaskFeatureNames(regr.task)[1:2])
  lrns = listLearnersForTask(task=task)
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    if (lrn$id == "regr.km")
      lrn = setHyperPars(lrn, nugget.estim=TRUE)
    m = train(lrn, task)
    p = predict(m, task)
  })

  # regr with se
  task = subsetTask(regr.task, subset=c(1:70),
  features = getTaskFeatureNames(regr.task)[1:2])
  lrns = listLearnersForTask(task=task, se=TRUE)
  lrns = lapply(lrns, makeLearner, predict.type="se")
  lapply(lrns, function(lrn) {
    if (lrn$id == "regr.km")
      lrn = setHyperPars(lrn, nugget.estim=TRUE)
    m = train(lrn, task)
    p = predict(m, task)
    expect_equal(length(p$data$se), 70)
  })

  # regr with weights
  task = subsetTask(regr.task, subset=1:70, features=getTaskFeatureNames(regr.task)[1:2])
  lrns = listLearnersForTask(task=task, weights=TRUE)
  lrns = lapply(lrns, makeLearner)
  lapply(lrns, function(lrn) {
    if (lrn$id == "regr.km")
      lrn = setHyperPars(lrn, nugget.estim=TRUE)
    m = train(lrn, task, weights=1:task$task.desc$size)
    p = predict(m, task)
  })
})

}
