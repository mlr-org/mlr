# this is a long test suite that is used to test the validity of ALL filters

context("filterFeatures")

test_that("filterFeatures", {
  # Loop through all filters
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  # univariate.model.score, permutation.importance and auc are handled extra test below
  # 'univariate', 'randomForest_importance' and 'rfsrc_var.select' are deprecated
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance", "auc",
    "univariate", "randomForest_importance", "randomForestSRC_var.select"))
  for (filter in filter.list.classif) {
    filterFeatures(task = multiclass.task, method = filter, perc = 0.5)
  }
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]
  for (filter in filter.list.regr) {
    filterFeatures(task = regr.num.task, method = filter, perc = 0.5)
  }

  fv = generateFilterValuesData(task = multiclass.task, method = "univariate.model.score", perc = 0.5,
    perf.learner = makeLearner("classif.rpart"), measures = mmce)
  expect_class(fv, classes = "FilterValues")
  expect_numeric(fv$data$value, any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(multiclass.task))

  # extra test of the permutation.importance filter
  fv = generateFilterValuesData(task = multiclass.task, method = "permutation.importance",
    imp.learner = makeLearner("classif.rpart"),
    measure = acc,
    contrast = function(x, y) abs(x - y),
    aggregation = median,
    nmc = 2L)
  expect_class(fv, classes = "FilterValues")
  expect_numeric(fv$data$value, any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(multiclass.task))

  # extra test for randomForestSRC_var.select filter (#1066)
  fv = suppressWarnings(generateFilterValuesData(task = multiclass.task, method = "randomForestSRC_var.select",
    more.args = list("randomForestSRC_var.select" = c(method = "vh", conservative = "low"))))
  expect_class(fv, classes = "FilterValues")

  # extra test for auc filter (two class dataset)
  toy.data = data.frame(
    Class = factor(c(1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L)),
    V1 = 1:8,
    V2 = 8:1,
    V3 = c(5L, 1L, 6L, 7L, 2L, 8L, 3L, 4L),
    V4 = c(1L, 5L, 2L, 3L, 6L, 4L, 7L, 8L),
    V5 = c(1L, 2L, 3L, 5L, 4L, 7L, 6L, 8L))
  toy.task = makeClassifTask("toy.task", data = toy.data, target = "Class", positive = 1L)

  fv = generateFilterValuesData(toy.task, method = "auc")
  expect_class(fv, classes = "FilterValues")
  expect_numeric(fv$data$value, any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(toy.task))
  expect_equal(fv$data$value, c(0.25, 0.25, 0.5, 0.5, 0.125))
})


test_that("randomForestSRC_var.select filter handles user choices correctly", {
  expect_silent(
    suppressWarnings(generateFilterValuesData(task = multiclass.task,
      method = "randomForestSRC_var.select",
      more.args = list("randomForestSRC_var.select" = c(method = "vh", conservative = "low"))))
  )

  # method = "vh.imp" is not supported
  expect_error(
    fv = suppressWarnings(generateFilterValuesData(task = multiclass.task,
      method = "randomForestSRC_var.select",
      more.args = list("randomForestSRC_var.select" = c(method = "vh.imp"))))
  )
})

test_that("randomForestSRC_var.select minimal depth filter returns NA for features below the threshold", {
  dat = generateFilterValuesData(task = multiclass.task,
    method = "randomForestSRC_var.select",
    nselect = 5,
    more.args = list("randomForestSRC_var.select" = list(method = "md", nrep = 5)))
  expect_equal(is.na(dat$data$value[dat$data$name %in% c("Sepal.Length", "Sepal.width")]), TRUE)
})

test_that("ensemble filters subset the task correctly", {

  # expectation for all filters was checked manually just right after the
  # internal aggregation (in filterFeatures.R)

  task.filtered = filterFeatures(bh.task,
    method = "E-mean",
    abs = 5,
    base.methods = c("univariate.model.score", "praznik_CMIM"))
  expect_equal(getTaskFeatureNames(task.filtered), c("indus", "nox", "rm", "ptratio", "lstat"))

  task.filtered = filterFeatures(bh.task,
    method = "E-min",
    abs = 5,
    base.methods = c("univariate.model.score", "praznik_CMIM"))
  expect_equal(getTaskFeatureNames(task.filtered), c("nox", "rm", "tax", "ptratio", "lstat"))

  task.filtered = filterFeatures(bh.task,
    method = "E-max",
    abs = 5,
    base.methods = c("univariate.model.score", "praznik_CMIM"))
  expect_equal(getTaskFeatureNames(task.filtered), c("indus", "nox", "rm", "ptratio", "lstat"))

  task.filtered = filterFeatures(bh.task,
    method = "E-median",
    abs = 5,
    base.methods = c("univariate.model.score", "praznik_CMIM"))
  expect_equal(getTaskFeatureNames(task.filtered), c("indus", "nox", "rm", "ptratio", "lstat"))

  task.filtered = filterFeatures(bh.task,
    method = "E-Borda",
    abs = 5,
    base.methods = c("univariate.model.score", "praznik_CMIM"))
  expect_equal(getTaskFeatureNames(task.filtered), c("indus", "nox", "rm", "ptratio", "lstat"))
})

test_that("Thresholding works with ensemble filters", {
  foo = filterFeatures(iris.task, method = "E-min",
    base.methods = c("FSelectorRcpp_gain.ratio", "FSelectorRcpp_information.gain"),
    thresh = 2)

  expect_equal(getTaskNFeats(foo), 3)
})
