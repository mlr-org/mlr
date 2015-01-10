context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  feat.imp = getFilterValues(binaryclass.task)
  expect_equal(ns, feat.imp$data$name)
  f = filterFeatures(binaryclass.task, select = "threshold", threshold = -Inf)
  expect_equal(f, binaryclass.task)

  feat.imp = getFilterValues(binaryclass.task, method = "chi.squared")
  expect_equal(ns, feat.imp$data$name)
  f = filterFeatures(binaryclass.task, method = "chi.squared", abs = 5L)
  expect_equal(getTaskFeatureNames(f), head(sortByCol(feat.imp$data, "val", asc = FALSE), 5L)$name)
  # now check that we get the same result by operating on getFilterValues
  feat.imp = getFilterValues(binaryclass.task)
  ff = filterFeatures(binaryclass.task, fval = feat.imp, abs = 5L)
  expect_equal(f, ff)
  
  f1 = filterFeatures(binaryclass.task, abs = 1L, mandatory.feat = "V1", ntree = 1L)
  f2 = subsetTask(binaryclass.task, features = "V1")
  expect_equal(f1, f2)

  f1 = filterFeatures(multiclass.task, abs = round(0.5 * ncol(multiclass.df)))
  f2 = filterFeatures(multiclass.task, perc = 0.5)
  expect_equal(f1, f2)

  lrn1 = makeFilterWrapper("classif.rpart", fw.perc = 0.2)
  m = train(lrn1, multiclass.task)
  f = getFilteredFeatures(m)
  expect_is(f, "character")
  expect_equal(length(f), 1L)

  # Loop through all filters
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  filter.list.classif = setdiff(filter.list.classif, c("univariate")) #make extra test
  for (filter in filter.list.classif) {
    filterFeatures(task = multiclass.task, method = filter, perc = 0.5)
  }
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]
  for (filter in filter.list.regr) {
    filterFeatures(task = regr.num.task, method = filter, perc = 0.5)
  }

  # extra test of univariate filter
  getFilterValues(task = multiclass.task, method = "univariate", perc = 0.5,
    perf.learner = makeLearner("classif.rpart"), measures = mmce)
})
