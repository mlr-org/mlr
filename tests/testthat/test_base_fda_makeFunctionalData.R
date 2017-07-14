context("makeFunctionalData")

test_that("makeFunctionalData works", {
  df = data.frame(matrix(rnorm(10^2), nrow = 10))
  df$fct = as.factor(letters[1:10])
  df$ord = as.ordered(1:10)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:5, "fd2" = 6:9))
  expect_equal(sapply(fdf, class)[[1]], "numeric")
  expect_equal(sapply(fdf, class)[[2]], "factor")
  expect_equal(sapply(fdf, class)[[3]], c("ordered", "factor"))
  expect_equal(sapply(fdf, class)[[4]], c("functional", "matrix"))
  expect_equal(sapply(fdf, class)[[5]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(10, 5))
  expect_class(fdf, "data.frame")
})

test_that("makeFunctionalData subsetting works", {
  df = data.frame(matrix(rnorm(10^2), nrow = 10))
  df$fct = as.factor(letters[1:10])
  df$ord = as.ordered(1:10)
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1:5, "fd2" = 6:9))

  # Subset rows
  fdf2 = fdf[1:5, , drop = FALSE]
  expect_equal(sapply(fdf2, class)[[1]], "numeric")
  expect_equal(sapply(fdf2, class)[[2]], "factor")
  expect_equal(sapply(fdf2, class)[[3]], c("ordered", "factor"))
  # FAILS:
  # expect_equal(sapply(fdf2, class)[[4]], c("functional", "matrix"))
  # expect_equal(sapply(fdf2, class)[[5]], c("functional", "matrix"))
  expect_equal(dim(fdf2), c(5, 5))
  expect_class(fdf2, "data.frame")

  # Subset cols
  fdf3 = fdf[, 2:4, drop = FALSE]
  expect_equal(sapply(fdf3, class)[[1]], "factor")
  expect_equal(sapply(fdf3, class)[[2]], c("ordered", "factor"))
  # FAILS:
  # expect_equal(sapply(fdf3, class)[[3]], c("functional", "matrix"))
  expect_equal(dim(fdf3), c(10, 3))
  expect_class(fdf3, "data.frame")
})

test_that("makeFunctionalData works for different inputs", {

  df = data.frame(matrix(rnorm(50), nrow = 5))
  # for 1-D matricies
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 2:10))
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(lapply(fdf, class)[[2]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")

  # for column name inputs
  fdf = makeFunctionalData(df, fd.features = list("fd1" = "X1", "fd2" = paste0("X", 2:10)))
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(lapply(fdf, class)[[2]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")

  # for empty lists
  fdf = makeFunctionalData(df, fd.features = list())
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 1))
  expect_class(fdf, "data.frame")

  # default
  fdf = makeFunctionalData(df)
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 1))
  expect_class(fdf, "data.frame")

  # data.frame already has matrix
  # FIXME: The colnames in prints are ugly.
  df2 = df[, 1, drop = FALSE]
  df2$fd1 = as.matrix(df[, 2:10])
  fdf = makeFunctionalData(df2)
  expect_equal(lapply(fdf, class)[[1]], "numeric")
  expect_equal(lapply(fdf, class)[[2]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")
})


test_that("makeFunctionalData works for different inputs", {

  df = data.frame(matrix(rnorm(50), nrow = 5))
  # for 1-D matricies
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 2:10))
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(lapply(fdf, class)[[2]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")

  # for column name inputs
  fdf = makeFunctionalData(df, fd.features = list("fd1" = "X1", "fd2" = paste0("X", 2:10)))
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(lapply(fdf, class)[[2]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")

  # for empty lists
  fdf = makeFunctionalData(df, fd.features = list())
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 1))
  expect_class(fdf, "data.frame")

  # default
  fdf = makeFunctionalData(df)
  expect_equal(lapply(fdf, class)[[1]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 1))
  expect_class(fdf, "data.frame")

  # data.frame already has matrix
  # FIXME: The colnames in prints are ugly.
  df2 = df[, 1, drop = FALSE]
  df2$fd1 = as.matrix(df[, 2:10])
  fdf = makeFunctionalData(df2)
  expect_equal(lapply(fdf, class)[[1]], "numeric")
  expect_equal(lapply(fdf, class)[[2]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")
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

  # FIXME: Functional class gets dropped because of subsetting rows
  subs.clt = subsetTask(clt, subset = c(2, 5))
  expect_class(subs.clt, c("ClassifTask", "SupervisedTask", "Task"))
  # expect_equal(subs.clt$task.desc$n.feat["functionals"], c("functionals" = 3L))
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

  # FIXME: Functional class gets dropped because of subsetting rows
  subs.clust1 = subsetTask(clustt, subset = 2:4)
  expect_class(subs.clust1, c("ClusterTask", "UnsupervisedTask", "Task"))
  # expect_equal(subs.clust1$task.desc$n.feat["functionals"], c("functionals" = 3L))
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
  expect_message({tdata1 = getTaskData(clt, keep.functionals = FALSE)}, "have been converted to numerics")
  expect_true(!("matrix" %in% sapply(tdata1, class)))
  expect_equal(tdata1[, getTaskTargetNames(clt)], as.factor(letters[1:5]))

  tdata2 = getTaskData(clt, keep.functionals = TRUE)
  expect_true("matrix" %in% unlist(sapply(tdata2, class)))
  expect_equal(tdata2[, getTaskTargetNames(clt)], as.factor(letters[1:5]))


  tdata3 = getTaskData(clt, keep.functionals = TRUE, target.extra = TRUE)
  expect_equal(tdata3$target, as.factor(letters[1:5]))
  expect_true("matrix" %in% unlist(sapply(tdata3$data, class)))

  expect_message({tdata4 = getTaskData(clt, keep.functionals = FALSE, target.extra = TRUE)})
  expect_true(!("matrix" %in% sapply(tdata4$data, class)))
  expect_equal(tdata4$target, as.factor(letters[1:5]))


  # For clustering task
  clustt = makeClusterTask(data = fdf)
  expect_message({tdatacl1 = getTaskData(clustt, keep.functionals = FALSE)}, "have been converted to numerics")
  expect_true(!("matrix" %in% sapply(tdatacl1, class)))
  tdatacl2 = getTaskData(clustt, keep.functionals = TRUE)
  expect_true("matrix" %in% unlist(sapply(tdatacl2, class)))
})

test_that("changeData for functionals", {

  df = data.frame(matrix(rnorm(50), nrow = 5))
  df$tcl = as.factor(letters[1:5])
  df$treg = 1:5
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 5:10, "fd3" = c("X2", "X3", "X4")))

  # After changeData, task stays the same
  clt = makeClassifTask(data = fdf, target = "tcl")
  tdata = getTaskData(clt, keep.functionals = TRUE)
  expect_equal(changeData(clt, tdata), clt)

  # FIXME: functional class is dropped in changeData
  subs.clt = changeData(clt, tdata[1:3, 1:2])
  expect_class(subs.clt, c("ClassifTask", "SupervisedTask", "Task"))
  # expect_equal(subs.clt$task.desc$n.feat["functionals"], c("functionals" = 1L))
  expect_equal(subs.clt$task.desc$n.feat["factors"], c("factors" = 0L))
  expect_equal(subs.clt$task.desc$n.feat["numerics"], c("numerics" = 1L))
  expect_equal(subs.clt$task.desc$n.feat["ordered"], c("ordered" = 0L))
  expect_equal(subs.clt$task.desc$size, 3L)
})

test_that("makeFunctionalData produces valid error messages", {

  df = data.frame("x" = 1:3, "y" = 2:4, "z" = letters[1:3])
  expect_error(makeFunctionalData(df, fd.features = list("fd1" = 1:4)), "Must be a subset of")
  expect_error(makeFunctionalData(df, fd.features = list("fd1" = 1:3)), "Must store numerics")

  # Technically we allow functional features of length >= 1
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 2))
  expect_class(fdf$fd1, c("functional", "matrix"))
  expect_class(fdf$fd2, c("functional", "matrix"))
  expect_class(fdf$z, c("factor"))
  fdf2 = makeFunctionalData(df, fd.features = list("fd1" = "x", "fd2" = "y"))
  expect_class(fdf2$fd1, c("functional", "matrix"))
  expect_class(fdf2$fd2, c("functional", "matrix"))
  expect_class(fdf2$z, c("factor"))
  expect_equal(fdf, fdf2)

  # Exclude.cols works
  expect_class(makeFunctionalData(df, fd.features = list(), exclude.cols = "z"), "data.frame")

  # Exclude.cols works for character
  fdf3 = makeFunctionalData(df, fd.features = list(), exclude.cols = c("z", "y"))
  expect_equal(sapply(fdf3, class)$z, "factor")
  expect_equal(sapply(fdf3, class)$fd1, c("functional", "matrix"))
  expect_equal(sapply(fdf3, class)$y, "integer")
  expect_equal(dim(fdf3$fd1), c(3, 1))

  # Exclude.cols works for integer
  fdf4 = makeFunctionalData(df, fd.features = list(), exclude.cols = c(3, 2))
  expect_equal(sapply(fdf4, class)$z, "factor")
  expect_equal(sapply(fdf4, class)$fd1, c("functional", "matrix"))
  expect_equal(sapply(fdf4, class)$y, "integer")
  expect_equal(dim(fdf4$fd1), c(3, 1))


  expect_error(makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 2), exclude.cols = "x"),
    "Matrix dimensions need to be")

  # Check if exclude.cols overwrites fd.features
  fdf5 = makeFunctionalData(df, fd.features = list("fd1" = 1:2), exclude.cols = "x")
  expect_equal(sapply(fdf5, class)$x, "integer")
  expect_equal(sapply(fdf5, class)$fd1, c("functional", "matrix"))
  expect_equal(sapply(fdf5, class)$z, "factor")
  expect_equal(dim(fdf5$fd1), c(3, 1))

  expect_error(makeFunctionalData(data.frame(matrix(letters[1:9], nrow = 3)),
    fd.features = list("fd1" = 1:3)), "Must store numerics")

  })

# test_that("Code from Bernd", {
  # d = list(x1 = matrix(123, 3, 2), x2 = matrix(1:6, 3, 2), x3 = 1:3)
  # class(d) = "data.frame"
  # names(d) = c("x1", "x2", "x3")
  # rownames(d) = 1:3
  # print(str(d))
  # sapply(d, class)
  #
  # x1 = matrix(123, 3, 2)
  # x1 = BBmisc::addClasses(x1, "functional")
  # x2 = matrix(1:6, 3, 2)
  # x2 = BBmisc::addClasses(x2, "functional")
  # d = list(x1 = x1, x2 = x2, x3 = 1:3)
  # class(d) = "data.frame"
  # names(d) = c("x1", "x2", "x3")
  # rownames(d) = 1:3
  # print(str(d))
  # sapply(d, class)
  # d = d[1:2, , drop = FALSE]
  # sapply(d, class)
# })
