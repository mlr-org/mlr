context("cpo impute")

test_that("CPO Impute data frame", {
  data = data.frame(f = letters[c(1, 1, 1, 1, 2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4), z = NA)
  target = "z"
  data[6, ] = NA

  # median
  imputed = data %>>% cpoImpute(target.cols = target, cols = list(x = imputeMedian(), y = imputeMedian()))
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # mode
  imputed = data %>>% cpoImpute(target.cols = target, cols = list(f = imputeMode(), x = imputeMode(), y = imputeMode()))
  expect_equal(as.character(imputed$f[6]), "a")
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # min / max
  imputed = data %>>% cpoImpute(target.cols = target, cols = list(x = imputeMin(2), y = imputeMax(0)))
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], max(data$y, na.rm = TRUE))
  imputed = data %>>% cpoImpute(target.cols = target, classes = list(numeric = imputeMax()))
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], max(data$y, na.rm = TRUE) + diff(range(data$y, na.rm = TRUE)))

  # normal
  imputed = data %>>% cpoImpute(target.cols = target, cols = list(x = imputeNormal(), y = imputeNormal()))
  expect_equal(imputed$x[6], 1)

  # hist / table
  imputed = data %>>% cpoImpute(target = target, cols = list(f = imputeHist(), x = imputeHist(), y = imputeHist(breaks = 1, use.mids = FALSE)))
  expect_true(imputed$f[6] %in% c("a", "b"))
  expect_equal(imputed$x[6], 0.5)
  expect_equal(imputed$x[6], 0.5)
  expect_true(imputed$y[6] >= 0 && imputed$y[6] <= 5)

  # learner
  data2 = data.frame(V1 = 1:10, V2 = 1:10, V3 = 1:10, col = factor(rep(1:2, c(3, 7))), z = 1:10)
  data2$V2[9:10] = NA
  data2$V3[1:2] = NA
  data2$col[8:10] = NA
  # we impute col
  # case 1: feature used for imputation (V1) does not have missings
  # used to check functionality with a learner that does not have property "missings" (see #1035)
  lrn = makeLearner("classif.lda")
  expect_false(hasLearnerProperties(lrn, "missings"))
  imputed = data2 %>>% cpoImpute(target.cols = target, cols = list(col = imputeLearner(lrn, features = "V1")))
  expect_true(all(imputed$col[8:10] == "2"))
  # case 2: feature used for imputation (V2) has missings only in rows where col has missings
  # in this case the imputation task does not have property "missings", but a learner with property "missings" is
  # required for imputation
  expect_error(data2 %>>% cpoImpute(target.cols = target, cols = list(col = imputeLearner("classif.lda", features = "V2"))), "used for imputation has/have missing values, but learner")
  lrn = makeLearner("classif.naiveBayes")
  expect_true(hasLearnerProperties(lrn, "missings"))
  imputed = data2 %>>% cpoImpute(target.cols = target, cols = list(col = imputeLearner(lrn, features = "V2")))
  expect_true(all(imputed$col[8:10] == "2"))
  # case 3: feature used for imputation (V3) has missings only in rows where col does not have missings
  # in this case the imputation task has property "missings"
  expect_error(data2 %>>% cpoImpute(target = target, cols = list(col = imputeLearner("classif.lda", features = "V3"))), "used for imputation has/have missing values, but learner")
  imputed = data2 %>>% cpoImpute(target.cols = target, cols = list(col = imputeLearner("classif.naiveBayes", features = "V3")))
  expect_true(all(imputed$col[8:10] == "2"))

  # we had an issue here (see #26) where e.g. imputation for integer/numeric features via a classif learner showed
  # inconsistent behavior and resulted in weird error messages
  data2$col2 = as.integer(data2$col)
  # case 1: impute an integer (data2$col2) with a classif learner (integers are coerced to factors by checkTaskData)
  # using learner classif.lvq1 because it doesn't work with integer targets (see #26)
  set.seed(getOption("mlr.debug.seed"))
  imputed = data2 %>>% cpoImpute(cols = list(col2 = imputeLearner("classif.lvq1", features = "V1")))
  expect_true(all(imputed$col2[8:10] == "2"))
  # case 2: impute a numeric (data$x) with a classif learner
  expect_error(data %>>% cpoImpute(target.cols = target, cols = list(x = imputeLearner("classif.naiveBayes"))), "Assertion on 'x' failed")
  # case 3: impute a factor (data$f) with a regr learner
  expect_error(data %>>% cpoImpute(target.cols = target, cols = list(f = imputeLearner("regr.rpart"))), "Assertion on 'f' failed")

  # constant replacements
  imputed = data %>>% cpoImpute(target.cols = target, cols = list(f = "xxx", x = 999, y = 1000))
  expect_equal(as.character(imputed$f[6]), "xxx")
  expect_equal(imputed$x[6], 999)
  expect_equal(imputed$y[6], 1000)

  # some reimputations
  x = data %>>% cpoImpute(target.cols = target, cols = list(f = "xxx", x = imputeMode(), y = imputeMax(2)), impute.new.levels = TRUE)
  ret = retrafo(x)
  retrafo(x) = NULL
  imputed = data %>>% ret
  expect_equal(x, imputed)
  expect_error(data.frame(f = factor("newlvl"), x = NA) %>>% ret, "column name mismatch")
  imputed = data.frame(f = factor("newlvl"), x = NA_real_, y = 1, z = NA) %>>% ret
  expect_equal(as.character(imputed$f), "xxx")
  expect_equal(imputed$x, 1)
  #FIXME: y was never in input data? therefore next test fails?
  # expect_equal(imputed$y, 8)

  x = data %>>% cpoImpute(target.cols = target, cols = list(f = "xxx"), impute.new.levels = FALSE)
  imputed = data.frame(f = factor("newlvl"), x = NA_real_, y = 1, z = NA) %>>% retrafo(x)
  expect_true(is.na(imputed$f))
  expect_true("xxx" %in% levels(imputed$f))

  # dummies
  x = data %>>% cpoImpute(target.col = target, dummy.cols = "x")
  expect_equal(x[["x.dummy"]], as.factor(c(rep(FALSE, 5), TRUE)))
  ret = retrafo(x)
  retrafo(x) = NULL
  expect_equal(data %>>% ret, x)
  x = data %>>% cpoImpute(target.col = target, dummy.cols = "x", dummy.type = "numeric")
  expect_equal(x[["x.dummy"]], as.numeric(c(rep(FALSE, 5), TRUE)))
  ret = retrafo(x)
  retrafo(x) = NULL
  expect_equal(data %>>% ret, x)


  x = data %>>% cpoImpute(target.col = target, dummy.classes = "numeric")
  expect_true(setequal(names(x), c(names(data), "x.dummy", "y.dummy")))
  x = data %>>% cpoImpute(target.col = target, dummy.classes = "numeric", dummy.cols = "z")
  expect_true(setequal(names(x), c(names(data), "x.dummy", "y.dummy", "z.dummy")))

  x = data %>>% cpoImpute(classes = list(factor = imputeMode(), numeric = imputeMedian(),
      integer = imputeMedian(), logical = imputeConstant(1)))
  expect_true(all(!is.na(x)))

  data2 = data[1:5, ]
  x = data2 %>>% cpoImpute(target = target, dummy.classes = c("numeric", "logical", "factor"), force.dummies = TRUE)
  expect_true(setequal(getRetrafoState(retrafo(x))$control$dummies, c("f", "x", "y")))
  x = data2 %>>% cpoImpute(target = target, dummy.classes = c("numeric", "logical", "factor"), force.dummies = FALSE)
  expect_true(setequal(getRetrafoState(retrafo(x))$control$dummies, character(0)))
})

test_that("CPO ImputeWrapper", {
  d = iris[seq(1, 150, 3), ]
  d[1, 1] = NA_real_
  task = makeClassifTask(data = d, target = "Species")
  lrn = cpoImputeAll(classes = list(numeric = imputeMedian())) %>>% makeLearner("classif.rpart")
  m = train(lrn, task)
  p = predict(m, task)
  expect_true(!any(is.na(p$data$response)))
  mm = getLearnerModel(m, more.unwrap = TRUE)
  expect_output(print(mm), "root")
  expect_is(mm, "rpart")
  mm = getLearnerModel(m, more.unwrap = FALSE)
  expect_output(print(mm), "Model")
  expect_is(mm, "WrappedModel")
  expect_match(lrn$id, "[.]impute$")
})

test_that("CPO Impute and reimpute task", {
  data = data.frame(f = letters[c(1, 1, 1, 1, 2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4))
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
  imputed = classif.tsk %>>% cpoImpute(cols = list(f = imputeConstant("c"), x = imputeMean(), y = imputeMode()))
  imputed.tsk.data = getTaskData(imputed)
  imputed.data = data %>>% cpoImpute(target.cols = "z",
    cols = list(f = imputeConstant("c"), x = imputeMean(), y = imputeMode()))
  test.tsk = makeClassifTask(data = data2, target = "z")
  imputed.desc = retrafo(imputed)
  imputed.test.tsk = test.tsk %>>% imputed.desc
  test.imputed = data2 %>>% imputed.desc
  retrafo(imputed.data) = NULL
  expect_identical(imputed.data, imputed.tsk.data)
  expect_equal(class(classif.tsk), class(imputed))
  expect_identical(test.imputed, imputed.test.tsk$env$data)
  expect_equal(class(test.tsk), class(imputed.test.tsk))
})

test_that("CPO Impute works on non missing data", { # we had issues here: 848,893
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
    imputed = data %>>% cpoImpute(cols = list(a = impute.method))
    retrafo(imputed) = NULL
    expect_equal(data, imputed)
  }
  # test it in resampling
  dat = data.frame(y = rnorm(10), a = c(NA, rnorm(9)), b = rnorm(10))
  task = makeRegrTask(data = dat, target = "y")
  implrn = imputeLearner(makeLearner("regr.rpart"))
  lrn = cpoImputeAll(cols = list(a = implrn)) %>>% makeLearner("regr.lm")
  holdout(lrn, task)
})


test_that("cpoImputeXXX work", { # we had issues here: 848,893

  data = data.frame(a = c(1, 1, 2), b = 1:3)
  data2 = data
  data2[[2]][1] = NA

  impute.methods = list(
    cpoImputeConstant(0),
    cpoImputeMedian(),
    cpoImputeMean(),
    cpoImputeMode(),
    cpoImputeMin(),
    cpoImputeMax(),
    cpoImputeUniform(),
    cpoImputeNormal(),
    cpoImputeHist(),
    cpoImputeLearner(learner = makeLearner("regr.rpart"), features = "a")
  )

  for (impute.method in impute.methods) {
    imputed = data %>>% impute.method
    retrafo(imputed) = NULL
    expect_equal(data, imputed)
    imputed = data2 %>>% impute.method
    expect_equal(imputed$b.dummy, factor(c(TRUE, FALSE, FALSE), levels = c(FALSE, TRUE)))
    expect_equal(imputed$a, c(1, 1, 2))
    expect_true(all(!is.na(imputed)))
    expect_equal((data %>>% retrafo(imputed))[c(1, 2)], data)
    imputed = data2 %>>% setHyperPars(impute.method, make.dummy.cols = FALSE)
    expect_equal(names(imputed), c("a", "b"))
    expect_equal(imputed$a, c(1, 1, 2))
    expect_true(all(!is.na(imputed)))
    expect_equal(data %>>% retrafo(imputed), data)
  }

  # test it in resampling
  dat = data.frame(y = rnorm(10), a = c(NA, rnorm(9)), b = rnorm(10))
  task = makeRegrTask(data = dat, target = "y")
  implrn = cpoImputeLearner(makeLearner("regr.rpart"))
  lrn = implrn %>>% makeLearner("regr.lm")
  holdout(lrn, task)

})


test_that("CPO Logicals are casted to factors instead of character (#1522)", {
  x = data.frame(a = c(TRUE, FALSE, NA))
  y = x %>>% cpoImpute(cols = list(a = imputeConstant("__miss__")))
  res = factor(c("TRUE", "FALSE", "__miss__"), levels = c("FALSE", "TRUE", "__miss__"))
  expect_equal(y$a, res)
})
