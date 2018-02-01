context("filterFeatures_fselector_rcpp")
  test_that("filterFeatures_fselector_rcpp",
    a = c(1, 2, 5.3, 6, -2, 4, 8.3, 9.2, 10.1)  # numeric vector
    b = c("one", "two", "three")  # character vector
    c = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)  # logical vector
    d = c(1L, 3L, 5L, 7L, 9L, 17L)
    f = rep(c("c1", "c2"), 9)
    df = data.frame(a = a, b = b, c = c, d = d, f = f)
    df = convertDataFrameCols(df, logicals.as.factor = TRUE)
    lapply(df, class)
    task = makeClassifTask(data = df, target = "f")
    generateFilterValuesData(task, method = "info.gain.fselector.rcpp", nselect = 2L)
    generateFilterValuesData(task, method = "gain.ratio.fselector.rcpp", nselect = 1L)
    generateFilterValuesData(task, method = "symuncert.fselector.rcpp", nselect = 1L)

    lrn = makeLearner("classif.randomForest")
    lrn = makeFilterWrapper(learner = lrn, fw.method = "info.gain.fselector.rcpp", fw.perc = 0.9)
    res = resample(learner = lrn, task = binaryclass.task, resampling = cv10, measures = list(mmce, timetrain), extract = getFilteredFeatures, show.info = FALSE)
    expect_true(TRUE)
})

