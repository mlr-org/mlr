context("fda")

test_that("makeFunctionalData works", {
  df = data.frame(matrix(rnorm(10^2), nrow = 10))
  df$fct = as.factor(letters[1:10])
  df$ord = as.ordered(1:10)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:5, "fd2" = 6:9))
  expect_equal(lapply(fdf, class)[[1]], "numeric")
  expect_equal(lapply(fdf, class)[[2]], "factor")
  expect_equal(lapply(fdf, class)[[3]], c("ordered", "factor"))
  expect_equal(lapply(fdf, class)[[4]], "matrix")
  expect_equal(lapply(fdf, class)[[5]], "matrix")
  expect_equal(dim(fdf), c(10, 5))
  expect_class(fdf, "data.frame")
})

test_that("FDA properties work", {
  df = data.frame(matrix(rnorm(100), nrow = 10), "target" = sample(letters[1:2], 10, replace = TRUE))
  # Transform to functional data (fd.features indicate column names or indices)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:6, "fd2" = 8:10))
  # Create a classification task
  tsk = makeClassifTask(data = fdf, target = "target")
  lrn = makeLearner("classif.fdausc.knn")
  # Error for multiple functional inputs
  expect_error(resample(lrn, subsetTask(tsk, features = 2:3), cv2), "more than one functional inputs")
  # Error for numeric inputs
  expect_error(train(lrn, subsetTask(tsk, features = 1:3)), "numeric inputs")
  expect_error(train(lrn, subsetTask(tsk, features = 1)), "numeric inputs")
  # No error for single functional
  expect_silent(train(lrn, subsetTask(tsk, features = 2)))
  expect_silent(train(lrn, subsetTask(tsk, features = 3)))
})

test_that("makeFunctionalData subsetting works", {
  df = data.frame(matrix(rnorm(10^2), nrow = 10))
  df$fct = as.factor(letters[1:10])
  df$ord = as.ordered(1:10)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:5, "fd2" = 6:9))

  # Subset rows
  fdf2 = fdf[1:5, , drop = FALSE]
  expect_equal(lapply(fdf2, class)[[1]], "numeric")
  expect_equal(lapply(fdf2, class)[[2]], "factor")
  expect_equal(lapply(fdf2, class)[[3]], c("ordered", "factor"))
  expect_equal(lapply(fdf2, class)[[4]], "matrix")
  expect_equal(lapply(fdf2, class)[[5]], "matrix")
  expect_equal(dim(fdf2), c(5, 5))
  expect_class(fdf2, "data.frame")

  # Subset cols
  fdf3 = fdf[, 2:4, drop = FALSE]
  expect_equal(lapply(fdf3, class)[[1]], "factor")
  expect_equal(lapply(fdf3, class)[[2]], c("ordered", "factor"))
  expect_equal(lapply(fdf3, class)[[3]], "matrix")
  expect_equal(dim(fdf3), c(10, 3))
  expect_class(fdf3, "data.frame")
})

test_that("makeFunctionalData works for different inputs", {
  df = data.frame(matrix(rnorm(50), nrow = 5))
  # for 1-D matricies
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 2:10))
  expect_equal(lapply(fdf, class)[[1]], "matrix")
  expect_equal(lapply(fdf, class)[[2]], "matrix")
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")

  # for column name inputs
  fdf = makeFunctionalData(df, fd.features = list("fd1" = "X1", "fd2" = paste0("X", 2:10)))
  expect_equal(lapply(fdf, class)[[1]], "matrix")
  expect_equal(lapply(fdf, class)[[2]], "matrix")
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")

  # for fd.features = NULL
  fdf = makeFunctionalData(df, fd.features = NULL)
  expect_equal(lapply(fdf, class)[[1]], "matrix")
  expect_equal(dim(fdf), c(5, 1))
  expect_class(fdf, "data.frame")

  # for fd.features is an empy list
  fdf = makeFunctionalData(df, fd.features = list())
  expect_equal(lapply(fdf, class)[[1]], "numeric")
  expect_equal(dim(fdf), c(5, 10))
  expect_class(fdf, "data.frame")

  # default
  fdf = makeFunctionalData(df)
  expect_equal(lapply(fdf, class)[[1]], "matrix")
  expect_equal(dim(fdf), c(5, 1))
  expect_class(fdf, "data.frame")

  # data.frame already has matrix
  # FIXME: The colnames in prints are ugly.
  df2 = df[, 1, drop = FALSE]
  df2$fd1 = as.matrix(df[, 2:10])
  fdf = makeFunctionalData(df2, fd.features = list("fd2" = "X1"))
  expect_equal(lapply(fdf, class)[[1]], "matrix")
  expect_equal(lapply(fdf, class)[[2]], "matrix")
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")
})

test_that("getFunctionalFeatures works for different inputs", {
  fdf = getFunctionalFeatures(gunpoint.task, features = getTaskNFeats(gunpoint.task))
  expect_class(fdf[[1]], "matrix")
  expect_data_frame(fdf, ncols = 1L, nrows = 200L)

  fdf2 = getTaskData(gunpoint.task, functionals.as = "matrix")
  fdf3 = getFunctionalFeatures(fdf2, features = getTaskNFeats(gunpoint.task))
  expect_class(fdf3[[1]], "matrix")
  expect_data_frame(fdf3, ncols = 1L, nrows = 200L)

  # Throws errors
  expect_error(getFunctionalFeatures(matrix("blub"), features = getTaskNFeats(gunpoint.task)))
  expect_error(getFunctionalFeatures(data.frame("blub"), features = getTaskNFeats(gunpoint.task)),
    "No functional features in the data")

  # Works for multiple functionals
  fdf4 = getFunctionalFeatures(fuelsubset.task, features = getTaskFeatureNames(fuelsubset.task))
  expect_class(fdf4[[1]], "matrix")
  expect_class(fdf4[[2]], "matrix")
  expect_data_frame(fdf4, ncols = 2L, nrows = 129L)

  # Params for task method work
  fdf = getFunctionalFeatures(fuelsubset.task, subset = 1:100, features = 1:2)
  expect_class(fdf[[1]], "matrix")
  expect_data_frame(fdf, ncols = 1L, nrows = 100L)
})

test_that("makeFunctionalData Tasks work", {
  df = data.frame(matrix(rnorm(50), nrow = 5))
  df$tcl = as.factor(letters[1:5])
  df$treg = 1:5
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 5:10, "fd3" = c("X2", "X3", "X4")))

  clt = makeClassifTask(data = fdf, target = "tcl")
  expect_class(clt, c("ClassifTask", "SupervisedTask", "Task"))
  expect_equal(clt$task.desc$n.feat["functionals"], c("functionals" = 3L))
  expect_equal(clt$task.desc$n.feat["numerics"], c("numerics" = 1L))

  subs.clt = subsetTask(clt, subset = c(2, 5), features = getTaskFeatureNames(clt))
  expect_class(subs.clt, c("ClassifTask", "SupervisedTask", "Task"))
  expect_equal(subs.clt$task.desc$n.feat["functionals"], c("functionals" = 3L))
  expect_equal(subs.clt$task.desc$n.feat["factors"], c("factors" = 0L))
  expect_equal(subs.clt$task.desc$n.feat["numerics"], c("numerics" = 1L))
  expect_equal(subs.clt$task.desc$n.feat["ordered"], c("ordered" = 0L))
  expect_equal(subs.clt$task.desc$size, 2L)

  subs.clt2 = subsetTask(clt, features = c(1, 3))
  expect_class(subs.clt2, c("ClassifTask", "SupervisedTask", "Task"))
  expect_equal(subs.clt2$task.desc$n.feat["functionals"], c("functionals" = 1L))
  expect_equal(subs.clt2$task.desc$n.feat["factors"], c("factors" = 0L))
  expect_equal(subs.clt2$task.desc$n.feat["numerics"], c("numerics" = 1L))
  expect_equal(subs.clt2$task.desc$n.feat["ordered"], c("ordered" = 0L))
  expect_equal(subs.clt2$task.desc$size, 5L)

  regt = makeRegrTask(data = fdf, target = "treg")
  expect_class(regt, c("RegrTask", "SupervisedTask", "Task"))
  expect_equal(regt$task.desc$n.feat["functionals"], c("functionals" = 3L))
  expect_equal(regt$task.desc$n.feat["factors"], c("factors" = 1L))

  subs.regt = subsetTask(regt, features = 1:2)
  expect_class(subs.regt, c("RegrTask", "SupervisedTask", "Task"))
  expect_equal(subs.regt$task.desc$n.feat["factors"], c("factors" = 1L))
  expect_equal(subs.regt$task.desc$n.feat["numerics"], c("numerics" = 0L))
  expect_equal(subs.regt$task.desc$n.feat["ordered"], c("ordered" = 0L))
  expect_equal(subs.regt$task.desc$n.feat["functionals"], c("functionals" = 1L))
  expect_equal(subs.regt$task.desc$size, 5L)

  clustt = makeClusterTask(data = fdf)
  expect_class(clustt, c("ClusterTask", "UnsupervisedTask", "Task"))
  expect_equal(clustt$task.desc$n.feat["functionals"], c("functionals" = 3L))
  expect_equal(clustt$task.desc$n.feat["factors"], c("factors" = 1L))
  expect_equal(clustt$task.desc$n.feat["numerics"], c("numerics" = 1L))

  subs.clust1 = subsetTask(clustt, subset = 2:4, features = getTaskFeatureNames(clustt))
  expect_class(subs.clust1, c("ClusterTask", "UnsupervisedTask", "Task"))
  expect_equal(subs.clust1$task.desc$n.feat["factors"], c("factors" = 1L))
  expect_equal(subs.clust1$task.desc$n.feat["numerics"], c("numerics" = 1L))
  expect_equal(subs.clust1$task.desc$n.feat["ordered"], c("ordered" = 0L))
  expect_equal(subs.clust1$task.desc$size, 3L)

  subs.clust2 = subsetTask(clustt, features = 2:4)
  expect_class(subs.clust2, c("ClusterTask", "UnsupervisedTask", "Task"))
  expect_equal(subs.clust2$task.desc$n.feat["functionals"], c("functionals" = 2L))
  expect_equal(subs.clust2$task.desc$n.feat["factors"], c("factors" = 0L))
  expect_equal(subs.clust2$task.desc$n.feat["numerics"], c("numerics" = 1L))
  expect_equal(subs.clust2$task.desc$n.feat["ordered"], c("ordered" = 0L))
  expect_equal(subs.clust2$task.desc$size, 5L)
})

test_that("getTaskData for functionals", {
  df = data.frame(matrix(rnorm(50), nrow = 5))
  df$tcl = as.factor(letters[1:5])
  df$treg = 1:5
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 5:10, "fd3" = c("X2", "X3", "X4")))

  # For a classification
  clt = makeClassifTask(data = fdf, target = "tcl")
  expect_message({
    tdata1 = getTaskData(clt, functionals.as = "dfcols")
  }, "have been converted to numerics")
  expect_true(!("matrix" %in% lapply(tdata1, class)))
  expect_equal(tdata1[, getTaskTargetNames(clt)], as.factor(letters[1:5]))

  tdata2 = getTaskData(clt, functionals.as = "matrix")
  expect_true("matrix" %in% unlist(lapply(tdata2, class)))
  expect_equal(tdata2[, getTaskTargetNames(clt)], as.factor(letters[1:5]))


  tdata3 = getTaskData(clt, functionals.as = "matrix", target.extra = TRUE)
  expect_equal(tdata3$target, as.factor(letters[1:5]))
  expect_true("matrix" %in% unlist(lapply(tdata3$data, class)))

  expect_message({
    tdata4 = getTaskData(clt, functionals.as = "dfcols", target.extra = TRUE)
  })
  expect_true(!("matrix" %in% lapply(tdata4$data, class)))
  expect_equal(tdata4$target, as.factor(letters[1:5]))

  # For clustering task
  clustt = makeClusterTask(data = fdf)
  expect_message({
    tdatacl1 = getTaskData(clustt, functionals.as = "dfcols")
  }, "have been converted to numerics")
  expect_true(!("matrix" %in% lapply(tdatacl1, class)))
  tdatacl2 = getTaskData(clustt, functionals.as = "matrix")
  expect_true("matrix" %in% unlist(lapply(tdatacl2, class)))
})

test_that("changeData for functionals", {
  df = data.frame(matrix(rnorm(50), nrow = 5))
  df$tcl = as.factor(letters[1:5])
  df$treg = 1:5
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 5:10, "fd3" = c("X2", "X3", "X4")))

  # After changeData, task stays the same
  clt = makeClassifTask(data = fdf, target = "tcl")
  tdata = getTaskData(clt, functionals.as = "matrix")
  expect_equal(changeData(clt, tdata), clt)

  subs.clt = changeData(clt, tdata[1:3, 1:3])
  expect_class(subs.clt, c("ClassifTask", "SupervisedTask", "Task"))
  expect_equal(subs.clt$task.desc$n.feat["functionals"], c("functionals" = 1L))
  expect_equal(subs.clt$task.desc$n.feat["factors"], c("factors" = 0L))
  expect_equal(subs.clt$task.desc$n.feat["numerics"], c("numerics" = 1L))
  expect_equal(subs.clt$task.desc$n.feat["ordered"], c("ordered" = 0L))
  expect_equal(subs.clt$task.desc$size, 3L)
})

test_that("makeFunctionalData produces valid error messages", {
  df = data.frame("x" = 1:3, "y" = 2:4, "z" = letters[1:3])
  expect_error(makeFunctionalData(df, fd.features = list("fd1" = 1:4)), "Must be a subset of")
  expect_error(makeFunctionalData(df, fd.features = list("fd1" = 1:3)), "contains non-integer")

  # Technically we allow functional features of length >= 1
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 2))
  expect_class(fdf$fd1, "matrix")
  expect_class(fdf$fd2, "matrix")
  expect_class(fdf$z, "factor")
  fdf2 = makeFunctionalData(df, fd.features = list("fd1" = "x", "fd2" = "y"))
  expect_class(fdf2$fd1, "matrix")
  expect_class(fdf2$fd2, "matrix")
  expect_class(fdf2$z, "factor")
  expect_equal(fdf, fdf2)

  # Exclude.cols works
  expect_class(makeFunctionalData(df, fd.features = list(), exclude.cols = "z"), "data.frame")

  # Exclude.cols works for character
  fdf3 = makeFunctionalData(df, fd.features = NULL, exclude.cols = c("z", "y"))
  cls = lapply(fdf3, class)
  expect_equal(cls[["z"]], "factor")
  expect_equal(cls[["fd1"]], "matrix")
  expect_equal(cls[["y"]], "integer")
  expect_equal(dim(fdf3$fd1), c(3, 1))

  # Exclude.cols works for integer
  fdf4 = makeFunctionalData(df, fd.features = NULL, exclude.cols = c(3, 2))
  expect_equal(lapply(fdf4, class)[["z"]], "factor")
  expect_equal(lapply(fdf4, class)[["fd1"]], "matrix")
  expect_equal(lapply(fdf4, class)[["y"]], "integer")
  expect_equal(dim(fdf4$fd1), c(3, 1))

  # Check if exclude.cols overwrites fd.features
  fdf5 = makeFunctionalData(df, fd.features = list("fd1" = 1:2), exclude.cols = "x")
  expect_equal(lapply(fdf5, class)[["z"]], "factor")
  expect_equal(lapply(fdf5, class)[["fd1"]], "matrix")
  expect_equal(lapply(fdf5, class)[["x"]], "integer")
  expect_equal(dim(fdf5$fd1), c(3, 1))

  expect_error(makeFunctionalData(data.frame(matrix(letters[1:9], nrow = 3)),
    fd.features = list("fd1" = 1:3)), "fd.features contains non-integer")
})

test_that("hasFunctionals works", {
  expect_false(hasFunctionalFeatures(iris.task))
  expect_false(hasFunctionalFeatures(iris.task$task.desc))
  expect_false(hasFunctionalFeatures(getTaskData(iris.task)))

  expect_true(hasFunctionalFeatures(fda.binary.gp.task))
  expect_true(hasFunctionalFeatures(fda.binary.gp.task$task.desc))
  expect_true(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, functionals.as = "matrix")))
})

test_that("getTaskData for functional tasks", {
  expect_true(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, functionals.as = "matrix")))
  expect_message({
    df = getTaskData(fda.binary.gp.task, subset = 1:50, functionals.as = "dfcols")
  })
  expect_false(hasFunctionalFeatures(df))

  # Subset rows
  expect_true(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, subset = 1:50, functionals.as = "matrix")))
  expect_message({
    df = getTaskData(fda.binary.gp.task, subset = 1:50, functionals.as = "dfcols")
  })
  expect_false(hasFunctionalFeatures(df))

  # We can not really subset cols for this task.
  expect_false(hasFunctionalFeatures(getTaskData(fda.regr.fs.task, features = 1, functionals.as = "matrix")))
  expect_true(hasFunctionalFeatures(getTaskData(fda.regr.fs.task, features = 2, functionals.as = "matrix")))
  expect_true(hasFunctionalFeatures(getTaskData(fda.regr.fs.task, features = 3, functionals.as = "matrix")))
  expect_silent({
    df = getTaskData(fda.regr.fs.task, features = 1, functionals.as = "dfcols")
  })
  expect_false(hasFunctionalFeatures(df))
  expect_message({
    df = getTaskData(fda.regr.fs.task, features = c(2, 3), functionals.as = "dfcols")
  })
  expect_false(hasFunctionalFeatures(df))


  expect_error(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, features = 2, functionals.as = "matrix")))
  expect_error(hasFunctionalFeatures(getTaskData(fda.binary.gp.task, features = 2, functionals.as = "dfcols")))

  expect_false(hasFunctionalFeatures(getTaskData(iris.task, functionals.as = "matrix")))
  expect_silent({
    df = getTaskData(iris.task, subset = 1:50, functionals.as = "matrix")
  })
  expect_false(hasFunctionalFeatures(df))
  expect_false(hasFunctionalFeatures(getTaskData(iris.task, functionals.as = "dfcols")))
  expect_silent({
    df = getTaskData(iris.task, subset = 1:50, functionals.as = "dfcols")
  })
  expect_false(hasFunctionalFeatures(df))
})

test_that("benchmarking on fda tasks works", {
  lrns = list(makeLearner("classif.fdausc.knn"), makeLearner("classif.rpart"), makeLearner("classif.featureless"))
  expect_message({
    bmr = benchmark(lrns, fda.binary.gp.task.small, cv2, measures = getDefaultMeasure(fda.binary.gp.task.small))
  }, "Functional features have been")
  expect_class(bmr, "BenchmarkResult")
  expect_equal(names(bmr$results$gp.fdf), c("classif.fdausc.knn", "classif.rpart", "classif.featureless"))
  expect_numeric(as.data.frame(bmr)$mmce, lower = 0L, upper = 1L)


  # Test benchmark mixed learners regression
  lrns2 = list(makeLearner("regr.FDboost"), makeLearner("regr.rpart"), makeLearner("regr.featureless"))
  # suppress fdboost warning
  expect_message({
    bmr2 = suppressWarnings(benchmark(lrns2, fda.regr.fs.task, hout, measures = getDefaultMeasure(fda.regr.fs.task)))
  }, "Functional features have been")
  expect_class(bmr2, "BenchmarkResult")
  expect_equal(names(bmr2$results$fs.fdf), c("regr.FDboost", "regr.rpart", "regr.featureless"))
  expect_numeric(as.data.frame(bmr2)$mse, lower = 0L, upper = Inf)
  expect_error(train(makeLearner("classif.fdausc.knn"), iris.task), "numeric inputs")
})

test_that("makeFunctionalData for matricies contained in data.frame", {
  df = getTaskData(fuelsubset.task, functionals.as = "matrix")
  df2 = makeFunctionalData(df, fd.features = list("UVVIS" = "UVVIS", "NIR" = "NIR"),
    exclude.cols = c("heatan", "h20"))
  expect_equivalent(df, df2)

  df = data.frame(matrix(rnorm(100), ncol = 10L))
  df$fd1 = matrix(rnorm(100), ncol = 10L)
  fdf = makeFunctionalData(df, fd.features = list("fd2" = 1:10))
  expect_data_frame(fdf, nrows = 10L, ncols = 2L)
  expect_true(all(colnames(fdf) == c("fd1", "fd2")))
})

test_that("Self-created data.frame's", {
  df = data.frame(matrix(rnorm(100), ncol = 10L))
  df$fd1 = matrix(rnorm(100), ncol = 10L)
  task = makeRegrTask(data = df, target = "X1")
  expect_class(task, "Task")
  expect_true(task$task.desc$n.feat["functionals"] == 1L)

  # Throws error for character / factor etc.
  df2 = data.frame(matrix(rnorm(100), ncol = 10L))
  df2$fd1 = matrix(rep("a", 100L), ncol = 10L)
  expect_error(makeRegrTask(data = df2, target = "X1"), regexp = "Unsupported feature type")

  df2 = data.frame(matrix(rnorm(100), ncol = 10L))
  df2$fd1 = matrix(as.factor(rep("a", 100L)), ncol = 10L)
  expect_error(makeRegrTask(data = df2, target = "X1"), regexp = "Unsupported feature type")
})

# Test whether we support stratification: #1669
test_that("supports stratification", {
  res = makeResampleDesc(method = "RepCV", predict = "test",
    stratify = TRUE,
    folds = 2L, reps = 2L)

  # resampling instances
  resinst = makeResampleInstance(res, gunpoint.task)
  expect_class(resinst, "ResampleInstance")
})
