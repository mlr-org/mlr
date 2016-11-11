# this is a long test suite that is used to test the validity of ALL filters

context("filterFeatures")

test_that("filterFeatures", {
  # Loop through all filters
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  # univariate.model.score and permutation.importance are handled extra test below
  # 'univariate', 'rf.importance' and 'rf.min.depth' are deprecated
  filter.list.classif = setdiff(filter.list.classif, c(
    "univariate.model.score", "permutation.importance",
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
})

