# this is a long test suite that is used to test the validity of ALL filters

context("filterFeatures")

test_that("filterFeatures", {
  # Loop through all filters
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  # univariate.model.score, permutation.importance and auc are handled extra test below
  # 'univariate', 'rf.importance' and 'rf.min.depth' are deprecated
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance", "auc",
    "univariate", "rf.importance", "rf.min.depth"))
  for (filter in filter.list.classif) {
    filterFeatures(task = multiclass.task, method = filter, perc = 0.5)
  }
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]
  for (filter in filter.list.regr) {
    filterFeatures(task = regr.num.task, method = filter, perc = 0.5)
  }

  # extra test of univariate filter
  fv = suppressWarnings(getFilterValues(task = multiclass.task, method = "univariate.model.score", perc = 0.5,
      perf.learner = makeLearner("classif.rpart"), measures = mmce))
  expect_class(fv, classes = "FilterValues")
  expect_numeric(fv$data[, 2L], any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(multiclass.task))

  fv = generateFilterValuesData(task = multiclass.task, method = "univariate.model.score", perc = 0.5,
    perf.learner = makeLearner("classif.rpart"), measures = mmce)
  expect_class(fv, classes = "FilterValues")
  expect_numeric(fv$data[, 3L], any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(multiclass.task))

  # extra test of the permutation.importance filter
  fv = generateFilterValuesData(task = multiclass.task, method = "permutation.importance",
    imp.learner = makeLearner("classif.rpart"),
    measure = acc,
    contrast = function(x, y) abs(x - y),
    aggregation = median,
    nmc = 2L)
  expect_class(fv, classes = "FilterValues")
  expect_numeric(fv$data[, 3L], any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(multiclass.task))

  # extra test for rf.min.depth filter (#1066)
  fv = suppressWarnings(generateFilterValuesData(task = multiclass.task, method = "rf.min.depth",
    more.args = list("rf.min.depth" = c(method = "vh", conservative = "low"))))
  expect_class(fv, classes = "FilterValues")
  expect_numeric(fv$data[, 3L], any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(multiclass.task))

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
  expect_numeric(fv$data$auc, any.missing = FALSE, all.missing = FALSE, len = getTaskNFeats(toy.task))
  expect_equal(fv$data$auc, c(0.25, 0.25, 0.5, 0.5, 0.125))
})
