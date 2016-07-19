context("impute")

test_that("Impute data frame", {
  data = data.frame(f = letters[c(1,1,1,1,2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4), z = NA)
  target = "z"
  data[6, ] = NA

  # median
  imputed = impute(data, target = target, cols = list(x = imputeMedian(), y = imputeMedian()))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # mode
  imputed = impute(data, target = target, cols = list(f = imputeMode(), x = imputeMode(), y = imputeMode()))$data
  expect_equal(as.character(imputed$f[6]), "a")
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # min / max
  imputed = impute(data, target = target, cols = list(x = imputeMin(2), y = imputeMax(0)))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], max(data$y, na.rm = TRUE))
  imputed = impute(data, target = target, classes = list(numeric = imputeMax()))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], max(data$y, na.rm = TRUE) + diff(range(data$y, na.rm = TRUE)))

  # normal
  imputed = impute(data, target = target, cols = list(x = imputeNormal(), y = imputeNormal()))$data
  expect_equal(imputed$x[6], 1)

  # hist / table
  imputed = impute(data, target = target, cols = list(f = imputeHist(), x = imputeHist(), y = imputeHist(breaks = 1, use.mids = FALSE)))$data
  expect_true(imputed$f[6] %in% c("a", "b"))
  expect_equal(imputed$x[6], 0.5)
  expect_equal(imputed$x[6], 0.5)
  expect_true(imputed$y[6] >= 0 && imputed$y[6] <= 5)

  # constant replacements
  imputed = impute(data, target = target, cols = list(f = "xxx", x = 999, y = 1000))$data
  expect_equal(as.character(imputed$f[6]), "xxx")
  expect_equal(imputed$x[6], 999)
  expect_equal(imputed$y[6], 1000)

  # some reimputations
  x = impute(data, target = target, cols = list(f = "xxx", x = imputeMode(), y = imputeMax(2)), impute.new.levels = TRUE)
  imputed = reimpute(data, x$desc)
  expect_equal(x$data, imputed)
  imputed = reimpute(data.frame(f = factor("newlvl"), x = NA), x$desc)
  expect_equal(as.character(imputed$f), "xxx")
  expect_equal(imputed$x, 1)
  #FIXME: y was never in input data? therefore next test fails?
  # expect_equal(imputed$y, 8)

  x = impute(data, target = target, cols = list(f = "xxx"), impute.new.levels = FALSE)
  imputed = reimpute(data.frame(f = factor("newlvl"), x = NA), x$desc)
  expect_true(is.na(imputed$f))
  expect_true("xxx" %in% levels(imputed$f))

  # dummies
  x = impute(data, target = target, dummy.cols = "x")
  expect_equal(x$data[["x.dummy"]], as.factor(c(rep(FALSE, 5), TRUE)))
  expect_equal(reimpute(data, x$desc), x$data)
  x = impute(data, target = target, dummy.cols = "x", dummy.type = "numeric")
  expect_equal(x$data[["x.dummy"]], as.numeric(c(rep(FALSE, 5), TRUE)))
  expect_equal(reimpute(data, x$desc), x$data)

  x = impute(data, target = target, dummy.classes = c("numeric"))$data
  expect_true(setequal(names(x), c(names(data), "x.dummy", "y.dummy")))
  x = impute(data, target = target, dummy.classes = c("numeric"), dummy.cols = "z")$data
  expect_true(setequal(names(x), c(names(data), "x.dummy", "y.dummy", "z.dummy")))

  x = impute(data, target = target, classes = list(factor = imputeMode(), numeric = imputeMedian(),
      integer = imputeMedian(), logical = imputeConstant(1)))
  expect_true(all(!is.na(x)))

  data2 = data[1:5, ]
  x = impute(data2, target = target, dummy.classes = c("numeric", "logical", "factor"), force.dummies = TRUE)
  expect_true(setequal(x$desc$dummies, c("f", "x", "y")))
  x = impute(data2, target = target, dummy.classes = c("numeric", "logical", "factor"), force.dummies = FALSE)
  expect_true(setequal(x$desc$dummies, character(0)))
})

test_that("Impute and reimpute task", {
  data = data.frame(f = letters[c(1,1,1,1,2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4))
  data[6L, ] = NA
  classif.tar = factor(c(rep(c("a", "b"), 3L)))
  regr.tar = rep(c(.1, .2), 3L)
  #additional data-frame to check reimpute
  data2 = data.frame(f = letters[c(2, 1, 1, 1, 1)], x = rep(2., 5), y = c(2, 4, 2, 3, 3))
  data2[6L, ] = NA
  data2$z = classif.tar
  #test classif task
  data$z = classif.tar
  data2$z = classif.tar
  classif.tsk = makeClassifTask(data = data, target = "z")
  imputed = impute(classif.tsk,
    cols = list(f = imputeConstant("c"), x = imputeMean(), y = imputeMode()))
  imputed.tsk.data = getTaskData(imputed$task)
  imputed.data = impute(data, target = "z",
    cols = list(f = imputeConstant("c"), x = imputeMean(), y = imputeMode()))$data
  test.tsk = makeClassifTask(data = data2, target = "z")
  imputed.desc = imputed$desc
  imputed.test.tsk = reimpute(test.tsk, imputed.desc)
  test.imputed = reimpute(data2, imputed.desc)
  expect_identical(imputed.data, imputed.tsk.data)
  expect_equal(class(classif.tsk), class(imputed$task))
  expect_identical(test.imputed, imputed.test.tsk$env$data)
  expect_equal(class(test.tsk), class(imputed.test.tsk))
})

test_that("ImputeWrapper", {
  d = iris[seq(1, 150, 3), ]
  d[1,1] = NA_real_
  task = makeClassifTask(data = d, target = "Species")
  lrn = makeImputeWrapper("classif.rpart", classes = list(numeric = imputeMedian()))
  m = train(lrn, task)
  p = predict(m, task)
  expect_true(!any(is.na(p$data$response)))
  mm = getLearnerModel(m, more.unwrap = TRUE)
  expect_output(print(mm), "root")
  expect_is(mm, "rpart")
  mm = getLearnerModel(m, more.unwrap = FALSE)
  expect_output(print(mm), "Model")
  expect_is(mm, "WrappedModel")
})
