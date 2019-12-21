context("getTaskData")

test_that("getTaskData", {
  df = getTaskData(multiclass.task)
  expect_equal(df, multiclass.df)
  df = getTaskData(multiclass.task, subset = 1:10, features = colnames(multiclass.df)[1:2])
  expect_equal(df, multiclass.df[1:10, c(1:2, 5)])

  # recode.target
  td = getTaskDesc(binaryclass.task)
  df = getTaskData(binaryclass.task, recode.target = "01")
  expect_equal(df[, 1:20], binaryclass.df[, 1:20])
  expect_true(is.numeric(df[, binaryclass.target]))
  expect_equal(sum(df[, binaryclass.target] == 1),
    sum(binaryclass.df[, binaryclass.target] == td$positive))
  expect_equal(sum(df[, binaryclass.target] == 0),
    sum(binaryclass.df[, binaryclass.target] == td$negative))

  df = getTaskData(binaryclass.task, recode.target = "-1+1")
  expect_equal(df[, 1:20], binaryclass.df[, 1:20])
  expect_true(is.numeric(df[, binaryclass.target]))
  expect_equal(sum(df[, binaryclass.target] == 1),
    sum(binaryclass.df[, binaryclass.target] == td$positive))
  expect_equal(sum(df[, binaryclass.target] == -1),
    sum(binaryclass.df[, binaryclass.target] == td$negative))

  df = getTaskData(multilabel.task, recode.target = "multilabel.factor")
  expect_true(all(sapply(df[, multilabel.target], is.factor)))
  expect_true(all(df[multilabel.small.inds, multilabel.target] == data.frame(
    y1 = as.factor(c(TRUE, FALSE, TRUE, TRUE)),
    y2 = as.factor(c(FALSE, TRUE, FALSE, FALSE)))
  ))
  expect_equal(rownames(df[multilabel.small.inds, multilabel.target]), c("1", "52", "53", "123"))

  df = getTaskData(binaryclass.task, subset = 1:150, features = colnames(binaryclass.df)[1:2])
  expect_equal(nrow(df), 150)
  expect_equal(ncol(df), 3)
  df = getTaskData(binaryclass.task, subset = 1:150, features = colnames(binaryclass.df)[1:2],
    recode.target = "01")
  expect_equal(nrow(df), 150)
  expect_equal(ncol(df), 3)

  x = getTaskData(multiclass.task, target.extra = TRUE)
  expect_equal(x$data[, 1:4], multiclass.df[, 1:4])
  expect_equal(x$target, multiclass.df[, multiclass.target])

  # getTaskData works with index vector
  df = getTaskData(binaryclass.task, subset = 1:150, features = 1:2)
  expect_equal(nrow(df), 150)
  expect_equal(ncol(df), 3)
})

test_that("getTaskData survival", {
  df = getTaskData(surv.task)
  expect_equal(df, surv.df)
  cn = colnames(surv.df)[3:4]
  df = getTaskData(surv.task, subset = 1:10, features = cn)
  expect_equal(df, surv.df[1:10, union(cn, surv.target)])

  x = getTaskData(surv.task, target.extra = TRUE)
  expect_true(setequal(names(x), c("data", "target")))
  expect_true(is.data.frame(x$data))
  expect_true(is.data.frame(x$target))
  expect_equal(dim(x$data), c(nrow(surv.df), ncol(surv.df) - 2))
  expect_equal(dim(x$target), c(nrow(surv.df), 2L))
  expect_equal(names(x$target), surv.target)
  expect_true(setequal(names(x$data), setdiff(names(surv.df), surv.target)))

  x = getTaskData(surv.task, target.extra = TRUE, recode.target = "surv")
  expect_true(survival::is.Surv(x$target))
  expect_equal(dim(x$target), c(nrow(surv.df), 2L))
})

test_that("getTaskData multilabel", {
  df = getTaskData(multilabel.task)
  expect_equal(df, multilabel.df)
  cn = colnames(multilabel.df)[3:4]
  df = getTaskData(multilabel.task, subset = 1:10, features = cn)
  expect_equal(df, multilabel.df[1:10, union(cn, multilabel.target)])

  x = getTaskData(multilabel.task, target.extra = TRUE)
  expect_true(setequal(names(x), c("data", "target")))
  expect_true(is.data.frame(x$data))
  expect_true(is.data.frame(x$target))
  expect_equal(dim(x$data), c(150L, 5L))
  expect_equal(dim(x$target), c(150L, 2L))
  expect_equal(names(x$target), multilabel.target)
  expect_true(setequal(names(x$data), setdiff(names(multilabel.df), multilabel.target)))

  x = getTaskData(multilabel.task, target.extra = TRUE)
  expect_equal(dim(x$target), c(150L, 2L))
})
