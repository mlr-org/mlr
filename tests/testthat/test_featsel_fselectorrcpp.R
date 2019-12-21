context("filterFeatures_fselectorrcpp")

test_that("filterFeatures_fselectorrcpp", {
  a = c(1, 2, 5.3, 6, -2, 4, 8.3, 9.2, 10.1) # numeric vector
  b = c("one", "two", "three") # character vector
  c = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE) # logical vector
  d = c(1L, 3L, 5L, 7L, 9L, 17L)
  f = rep(c("c1", "c2"), 9)
  df = data.frame(a = a, b = b, c = c, d = d, f = f)
  df = convertDataFrameCols(df, logicals.as.factor = TRUE)
  task = makeClassifTask(data = df, target = "f")

  candidates = as.character(listFilterMethods()$id)
  candidates = candidates[startsWith(candidates, "FSelectorRcpp")]
  for (candidate in candidates) {
    fv = generateFilterValuesData(task, method = candidate, nselect = 2L)
    expect_class(fv, "FilterValues")
    expect_data_frame(fv$data, nrow = getTaskNFeats(task))
    expect_set_equal(fv$data$name, getTaskFeatureNames(task))
    expect_numeric(fv$data$value, any.missing = FALSE, lower = 0, finite = TRUE)
  }

  lrn = makeLearner("classif.rpart")
  lrn = makeFilterWrapper(learner = lrn,
    fw.method = "FSelectorRcpp_information.gain", fw.perc = 0.1)
  res = resample(learner = lrn, task = binaryclass.task, resampling = cv3,
    measures = list(mmce, timetrain), extract = getFilteredFeatures,
    show.info = FALSE)
  expect_length(res$extract[[1L]], 6)
})
