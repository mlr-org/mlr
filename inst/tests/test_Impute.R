context("Impute")

test_that("Impute data frame", {
  data = data.frame(f = letters[c(1,1,1,1,2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4), z = NA)
  target = "z"
  data[6, ] = NA

  # median
  imputed = impute(data, target, cols=list(x = imputeMedian(), y = imputeMedian()))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # mode
  imputed = impute(data, target, cols=list(f = imputeMode(), x = imputeMode(), y = imputeMode()))$data
  expect_equal(as.character(imputed$f[6]), "a")
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # min / max
  imputed = impute(data, target, cols=list(x = imputeMin(2), y = imputeMax(2)))$data
  expect_equal(imputed$x[6], 2)
  expect_equal(imputed$y[6], 8)

  # normal
  imputed = impute(data, target, cols=list(x = imputeNormal(), y = imputeNormal()))$data
  expect_equal(imputed$x[6], 1)

  # hist / table
  imputed = impute(data, target, cols=list(f = imputeHist(), x = imputeHist(), y = imputeHist(breaks=1, use.mids=FALSE)))$data
  expect_true(imputed$f[6] %in% c("a", "b"))
  expect_equal(imputed$x[6], 0.5)
  expect_equal(imputed$x[6], 0.5)
  expect_true(imputed$y[6] >= 0 && imputed$y[6] <= 5)

  # constant replacements
  imputed = impute(data, target, cols=list(f = "xxx", x = 999, y = 1000))$data
  expect_equal(as.character(imputed$f[6]), "xxx")
  expect_equal(imputed$x[6], 999)
  expect_equal(imputed$y[6], 1000)

  # some reimputations
  x = impute(data, target, cols=list(f = "xxx", x = imputeMode(), y = imputeMax(2)), impute.new.levels=TRUE)
  imputed = reimpute(data, x$desc)
  expect_equal(x$data, imputed)
  imputed = reimpute(data.frame(f=factor("newlvl"), x=NA), x$desc)
  expect_equal(as.character(imputed$f), "xxx")
  expect_equal(imputed$x, 1)
  #FIXME: y was never in input data? therefore next test fails?
  # expect_equal(imputed$y, 8)

  x = impute(data, target, cols=list(f = "xxx"), impute.new.levels=FALSE)
  imputed = reimpute(data.frame(f=factor("newlvl"), x=NA), x$desc)
  expect_true(is.na(imputed$f))
  expect_true("xxx" %in% levels(imputed$f))

  # dummies
  x = impute(data, target, dummy.cols="x")
  expect_equal(x$data[["x.dummy"]], c(rep(FALSE, 5), TRUE))
  expect_equal(reimpute(data, x$desc), x$data)

  # dummies
  x = impute(data, target, classes=list(factor=imputeMode(), numeric=imputeMedian(), integer=imputeMedian()))

  # learner
  data = data.frame(f = letters[c(1,1,1,1,2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4))
  target = "f"
  data[6, ] = NA
  learner = makeLearner("regr.rpart")
  x = impute(data, target, cols=list(x = imputeLearner(learner, preimpute=list(classes=list(numeric=imputeMedian())))))
})
