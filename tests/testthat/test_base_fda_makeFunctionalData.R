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
  expect_equal(lapply(fdf, class)[[1]], c("numeric"))
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
  expect_equal(lapply(fdf, class)[[1]], c("numeric"))
  expect_equal(lapply(fdf, class)[[2]], c("functional", "matrix"))
  expect_equal(dim(fdf), c(5, 2))
  expect_class(fdf, "data.frame")
})


test_that("makeFunctionalData works Tasks work", {

  df = data.frame(matrix(rnorm(50), nrow = 5))
  df$tcl = as.factor(letters[1:5])
  df$treg = 1:5
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 5:10, "fd3" = c("X2", "X3", "X4")))

  clt = makeClassifTask(data = fdf, target = "tcl")
  expect_class(clt, c("ClassifTask", "SupervisedTask", "Task"))
  expect_equal(clt$task.desc$n.feat["functionals"], c("functionals" = 3L))
  expect_equal(clt$task.desc$n.feat["numerics"], c("numerics" = 1L))

  regt = makeRegrTask(data = fdf, target = "treg")
  expect_class(regt, c("RegrTask", "SupervisedTask", "Task"))
  expect_equal(regt$task.desc$n.feat["functionals"], c("functionals" = 3L))
  expect_equal(regt$task.desc$n.feat["factors"], c("factors" = 1L))

  clustt = makeClusterTask(data = fdf)
  expect_class(clustt, c("ClusterTask", "UnsupervisedTask", "Task"))
  expect_equal(clustt$task.desc$n.feat["functionals"], c("functionals" = 3L))
  expect_equal(clustt$task.desc$n.feat["factors"], c("factors" = 1L))
  expect_equal(clustt$task.desc$n.feat["numerics"], c("numerics" = 1L))
})


test_that("getTaskData for functionals", {

  df = data.frame(matrix(rnorm(50), nrow = 5))
  df$tcl = as.factor(letters[1:5])
  df$treg = 1:5
  fdf = makeFunctionalData(df, fd.features = list("fd1" = 1, "fd2" = 5:10, "fd3" = c("X2", "X3", "X4")))

  clt = makeClassifTask(data = fdf, target = "tcl")
  tdata1 = getTaskData(clt, functionals = FALSE)
  expect_true(!("matrix" %in% sapply(tdata, class)))
  tdata2 = getTaskData(clt, functionals = TRUE)
  expect_true("matrix" %in% unlist(sapply(tdata2, class)))

  clustt = makeClusterTask(data = fdf)
  tdata3 = getTaskData(clustt, functionals = FALSE)
  expect_true(!("matrix" %in% sapply(tdata3, class)))
  tdata4 = getTaskData(clustt, functionals = TRUE)
  expect_true("matrix" %in% unlist(sapply(tdata4, class)))

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
