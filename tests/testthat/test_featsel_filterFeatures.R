context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  feat.imp = getFilterValues(binaryclass.task)
  expect_equal(ns, feat.imp$data$name)
  f = filterFeatures(binaryclass.task, select = "threshold", val = -Inf)
  expect_equal(f, binaryclass.task)

  feat.imp = getFilterValues(binaryclass.task, method = "chi.squared")
  expect_equal(ns, feat.imp$data$name)
  f = filterFeatures(binaryclass.task, method = "chi.squared", select = "abs", val = 5L)
  expect_equal(getTaskFeatureNames(f), head(sortByCol(feat.imp$data, "val", asc = FALSE), 5L)$name)

  f1 = filterFeatures(multiclass.task, select = "abs", val = round(0.5 * ncol(multiclass.df)))
  f2 = filterFeatures(multiclass.task, val = 0.5)
  expect_equal(f1, f2)

  lrn1 = makeFilterWrapper("classif.rpart", fw.val = 0.2)
  m = train(lrn1, multiclass.task)
  res = getFilterResult(m)
  expect_is(res, "FilterResult")
  expect_is(res, "character")
  expect_equal(length(res), 1L)

  lrn2 = makeFilterWrapper("classif.randomForest", fw.val = 0.5)
  bench = benchmark(learners = list(lrn1, lrn2), tasks = multiclass.task, resampling = makeResampleDesc("CV", iters = 3))
  res = getFilterResult(bench)
  # FIXME: why is this a list?
  expect_is(res, "list")
  res = res[[1]]
  expect_is(res, "list")
  expect_true(setequal(names(res), c("classif.rpart.filtered", "classif.randomForest.filtered")))
  res = res[[1]]
  expect_is(res[[1]], "FilterResult")
  
  ## Loop through all filters
  filter.list = listFilterMethods(desc = FALSE, tasks = TRUE, features = FALSE)
  filter.list.classif = as.character(filter.list$id)[filter.list$task.classif]
  filter.list.classif = setdiff(filter.list.classif, "univariate") #make extra test
  for (filter in filter.list.classif) {
    filterFeatures(task = multiclass.task, method = filter, val = 0.5)
  }
  filter.list.regr = as.character(filter.list$id)[!filter.list$task.classif & filter.list$task.regr]
  for (filter in filter.list.regr) {
    filterFeatures(task = regr.task, method = filter, val = 0.5)
  }
  
  ## extra test of univariate filter
  filterFeatures(task = multiclass.task, method = "univariate", val = 0.5, learner = makeLearner("classif.rpart"), measures = mmce)
})
