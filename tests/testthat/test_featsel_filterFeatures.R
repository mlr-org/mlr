context("filterFeatures")

test_that("filterFeatures", {
  ns = getTaskFeatureNames(binaryclass.task)
  f = filterFeatures(binaryclass.task, select = "threshold", threshold = -Inf)
  expect_equal(f, binaryclass.task)

  feat.imp.old = suppressWarnings(getFilterValues(binaryclass.task))
  expect_equal(ns, feat.imp.old$data$name)

  feat.imp.new = generateFilterValuesData(binaryclass.task)
  expect_equal(ns, feat.imp.new$data$name)

  feat.imp.old = suppressWarnings(getFilterValues(binaryclass.task, method = "chi.squared"))
  expect_equal(ns, feat.imp.old$data$name)
  f = filterFeatures(binaryclass.task, method = "chi.squared", abs = 5L)
  expect_true(setequal(getTaskFeatureNames(f), head(sortByCol(feat.imp.old$data, "val", asc = FALSE), 5L)$name))
  # now check that we get the same result by operating on getFilterValues
  feat.imp.old = suppressWarnings(getFilterValues(binaryclass.task, method = "chi.squared"))
  ff = filterFeatures(binaryclass.task, fval = feat.imp.old, abs = 5L)
  expect_equal(f, ff)

  feat.imp.new = generateFilterValuesData(binaryclass.task, method = "chi.squared")
  expect_equal(ns, feat.imp.new$data$name)
  f = filterFeatures(binaryclass.task, method = "chi.squared", abs = 5L)
  expect_true(setequal(getTaskFeatureNames(f),
      head(sortByCol(feat.imp.new$data, "chi.squared", asc = FALSE), 5L)$name))
  # now check that we get the same result by operating on generateFilterValuesData
  feat.imp.new = generateFilterValuesData(binaryclass.task, method = "chi.squared")
  ff = filterFeatures(binaryclass.task, fval = feat.imp.new, abs = 5L)
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
  # univariate.model.score and permutation.importance are handled extra test below
  # 'univariate' is deprecated
  filter.list.classif = setdiff(filter.list.classif, c("univariate.model.score", "permutation.importance", "univariate"))
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
                                learner = makeLearner("classif.rpart"),
                                measure = acc,
                                contrast = function(x, y) abs(x - y),
                                aggregation = median,
                                nperm = 2)
})

test_that("plotFilterValues", {
  fv = generateFilterValuesData(binaryclass.task, method = "chi.squared")
  plotFilterValues(fv)
  dir = tempdir()
  path = paste0(dir, "/test.svg")
  ggsave(path)
  doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, black.bar.xpath, ns.svg)), equals(20))
  ## plotFilterValuesGGVIS(fv)

  fv2 = generateFilterValuesData(binaryclass.task, method = c("chi.squared", "rf.importance"))
  plotFilterValues(fv2)
  ggsave(path)
  doc = XML::xmlParse(path)
  #expect_that(length(XML::getNodeSet(doc, black.bar.xpath, ns.svg)), equals(40))
  #expect_that(length(XML::getNodeSet(doc, grey.xpath, ns.svg)), equals(ncol(fv2$data) - 2))
  ## plotFilterValuesGGVIS(fv2)
})
