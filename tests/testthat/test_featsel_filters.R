# this is a long test suite that is used to test the validity of ALL filters

context("filterFeatures")

test_that("filterFeatures", {
  # Loop through all filters
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  # univariate.model.score and permutation.importance are handled extra test 
  # below 'univariate' is deprecated
  # rf.* tests removed to avoid sequential testing errors
  filter.list.classif = setdiff(filter.list.classif, c("univariate.model.score", 
                                                       "permutation.importance", 
                                                       "univariate",
                                                       "rf.importance",
                                                       "rf.min.depth"))
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
  fv = generateFilterValuesData(task = multiclass.task, method = "univariate.model.score", perc = 0.5,
    perf.learner = makeLearner("classif.rpart"), measures = mmce)

  # extra test of the permutation.importance filter
  fv = generateFilterValuesData(task = multiclass.task, method = "permutation.importance",
    imp.learner = makeLearner("classif.rpart"),
    measure = acc,
    contrast = function(x, y) abs(x - y),
    aggregation = median,
    nperm = 2)
})

