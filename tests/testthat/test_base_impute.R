context("impute")

test_that("Impute data frame", {
  data = data.frame(f = letters[c(1, 1, 1, 1, 2)], x = rep(1., 5),
    y = c(1, 2, 3, 3, 4), z = NA)
  target = "z"
  data[6, ] = NA

  # median
  imputed = impute(data, target = target, cols = list(x = imputeMedian(),
    y = imputeMedian()))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # mode
  imputed = impute(data, target = target, cols = list(f = imputeMode(),
    x = imputeMode(), y = imputeMode()))$data
  expect_equal(as.character(imputed$f[6]), "a")
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # min / max
  imputed = impute(data, target = target, cols = list(x = imputeMin(2),
    y = imputeMax(0)))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], max(data$y, na.rm = TRUE))
  imputed = impute(data, target = target, classes = list(numeric = imputeMax()))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], max(data$y, na.rm = TRUE) + diff(range(data$y, na.rm = TRUE)))

  # normal
  imputed = impute(data, target = target, cols = list(x = imputeNormal(),
    y = imputeNormal()))$data
  expect_equal(imputed$x[6], 1)

  # hist / table
  imputed = impute(data, target = target, cols = list(f = imputeHist(),
    x = imputeHist(), y = imputeHist(breaks = 1, use.mids = FALSE)))$data
  expect_true(imputed$f[6] %in% c("a", "b"))
  expect_equal(imputed$x[6], 0.5)
  expect_equal(imputed$x[6], 0.5)
  expect_true(imputed$y[6] >= 0 && imputed$y[6] <= 5)

  # learner
  data2 = data.frame(V1 = 1:10, V2 = 1:10, V3 = 1:10,
    col = factor(rep(1:2, c(3, 7))), z = 1:10)
  data2$V2[9:10] = NA
  data2$V3[1:2] = NA
  data2$col[8:10] = NA

  # we impute col case 1: feature used for imputation (V1) does not have
  # missings used to check functionality with a learner that does not have
  # property "missings" (see #1035)
  lrn = makeLearner("classif.lda")
  expect_false(hasLearnerProperties(lrn, "missings"))
  imputed = impute(data2, target = target, cols = list(col = imputeLearner(lrn, features = "V1")))$data
  expect_true(all(imputed$col[8:10] == "2"))

  # case 2: feature used for imputation (V2) has missings only in rows where col
  # has missings in this case the imputation task does not have property
  # "missings", but a learner with property "missings" is required for
  # imputation
  expect_error(impute(data2, target = target, cols = list(col = imputeLearner("classif.lda", features = "V2"))), "used for imputation has/have missing values, but learner")
  lrn = makeLearner("classif.naiveBayes")
  expect_true(hasLearnerProperties(lrn, "missings"))
  imputed = impute(data2, target = target, cols = list(col = imputeLearner(lrn, features = "V2")))$data
  expect_true(all(imputed$col[8:10] == "2"))

  # case 3: feature used for imputation (V3) has missings only in rows where col
  # does not have missings in this case the imputation task has property
  # "missings"
  expect_error(impute(data2, target = target, cols = list(col = imputeLearner("classif.lda", features = "V3"))), "used for imputation has/have missing values, but learner")
  imputed = impute(data2, target = target, cols = list(col = imputeLearner("classif.naiveBayes", features = "V3")))$data
  expect_true(all(imputed$col[8:10] == "2"))

  # we had an issue here (see #26) where e.g. imputation for integer/numeric
  # features via a classif learner showed inconsistent behavior and resulted in
  # weird error messages
  data2$col2 = as.integer(data2$col)

  # case 1: impute an integer (data2$col2) with a classif learner (integers are
  # coerced to factors by checkTaskData) using learner classif.lvq1 because it
  # doesn't work with integer targets (see #26)
  set.seed(getOption("mlr.debug.seed"))
  imputed = impute(data2, cols = list(col2 = imputeLearner("classif.lvq1",
    features = "V1")))$data
  expect_true(all(imputed$col2[8:10] == "2"))
  # case 2: impute a numeric (data$x) with a classif learner
  expect_error(impute(data, target = target, cols = list(x = imputeLearner("classif.naiveBayes"))), "Assertion on 'x' failed")
  # case 3: impute a factor (data$f) with a regr learner
  expect_error(impute(data, target = target, cols = list(f = imputeLearner("regr.rpart"))), "Assertion on 'f' failed")

  # constant replacements
  imputed = impute(data, target = target, cols = list(f = "xxx", x = 999, y = 1000))$data
  expect_equal(as.character(imputed$f[6]), "xxx")
  expect_equal(imputed$x[6], 999)
  expect_equal(imputed$y[6], 1000)

  # some reimputations
  x = impute(data, target = target, cols = list(f = "xxx", x = imputeMode(),
    y = imputeMax(2)), impute.new.levels = TRUE)
  imputed = reimpute(data, x$desc)
  expect_equal(x$data, imputed)
  expect_error(reimpute(data.frame(f = factor("newlvl"), x = NA), x$desc), "column types have changed")
  imputed = reimpute(data.frame(f = factor("newlvl"), x = NA_real_), x$desc)
  expect_equal(as.character(imputed$f), "xxx")
  expect_equal(imputed$x, 1)
  # FIXME: y was never in input data? therefore next test fails?
  # expect_equal(imputed$y, 8)

  x = impute(data, target = target, cols = list(f = "xxx"), impute.new.levels = FALSE)
  imputed = reimpute(data.frame(f = factor("newlvl"), x = NA_real_), x$desc)
  expect_true(is.na(imputed$f))
  expect_true("xxx" %in% levels(imputed$f))

  # dummies
  x = impute(data, target = target, dummy.cols = "x")
  expect_equal(x$data[["x.dummy"]], as.factor(c(rep(FALSE, 5), TRUE)))
  expect_equal(reimpute(data, x$desc), x$data)
  x = impute(data, target = target, dummy.cols = "x", dummy.type = "numeric")
  expect_equal(x$data[["x.dummy"]], as.numeric(c(rep(FALSE, 5), TRUE)))
  expect_equal(reimpute(data, x$desc), x$data)

  x = impute(data, target = target, dummy.classes = "numeric")$data
  expect_true(setequal(names(x), c(names(data), "x.dummy", "y.dummy")))
  x = impute(data, target = target, dummy.classes = "numeric", dummy.cols = "z")$data
  expect_true(setequal(names(x), c(names(data), "x.dummy", "y.dummy", "z.dummy")))

  x = impute(data, target = character(0), classes = list(factor = imputeMode(), numeric = imputeMedian(),
    integer = imputeMedian(), logical = imputeConstant(1)))
  expect_true(all(!is.na(x$data)))

  data2 = data[1:5, ]
  x = impute(data2, target = target, dummy.classes = c("numeric", "logical", "factor"), force.dummies = TRUE)
  expect_true(setequal(x$desc$dummies, c("f", "x", "y")))
  x = impute(data2, target = target, dummy.classes = c("numeric", "logical", "factor"), force.dummies = FALSE)
  expect_true(setequal(x$desc$dummies, character(0)))
})

test_that("Impute and reimpute task", {
  data = data.frame(f = letters[c(1, 1, 1, 1, 2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4))
  data[6L, ] = NA
  classif.tar = factor(c(rep(c("a", "b"), 3L)))
  regr.tar = rep(c(.1, .2), 3L)
  # additional data-frame to check reimpute
  data2 = data.frame(f = letters[c(2, 1, 1, 1, 1)], x = rep(2., 5), y = c(2, 4, 2, 3, 3))
  data2[6L, ] = NA
  data2$z = classif.tar
  # test classif task
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
  d[1, 1] = NA_real_
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
  expect_match(lrn$id, "[.]imputed$")
})

test_that("Impute works on non missing data", { # we had issues here: 848,893
  data = data.frame(a = c(1, 1, 2), b = 1:3)
  impute.methods = list(
    imputeConstant(0),
    imputeMedian(),
    imputeMean(),
    imputeMode(),
    imputeMin(),
    imputeMax(),
    imputeUniform(),
    imputeNormal(),
    imputeHist(),
    imputeLearner(learner = makeLearner("regr.fnn"))
  )
  for (impute.method in impute.methods) {
    imputed = impute(data, cols = list(a = impute.method))$data
    expect_equal(data, imputed)
  }
  # test it in resampling
  dat = data.frame(y = rnorm(10), a = c(NA, rnorm(9)), b = rnorm(10))
  task = makeRegrTask(data = dat, target = "y")
  implrn = imputeLearner(makeLearner("regr.rpart"))
  lrn = makeImputeWrapper(makeLearner("regr.lm"), cols = list(a = implrn))
  holdout(lrn, task)
})

test_that("Logicals are casted to factors instead of character (#1522)", {
  x = data.frame(a = c(TRUE, FALSE, NA))
  y = impute(x, cols = list(a = imputeConstant("__miss__")))
  res = factor(c("TRUE", "FALSE", "__miss__"), levels = c("FALSE", "TRUE", "__miss__"))
  expect_equal(y$data$a, res)
})
