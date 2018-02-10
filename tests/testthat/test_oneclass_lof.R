context("oneclass_lof")

test_that("oneclass_lof", {
  requirePackagesOrSkip("dbscan", default.method = "load")

  parset.list = list(
    list(k = 20),
    list(k = 50),
    list(k = 100)
  )

  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    # for lof, there is no seperation of test and train data
    # here: only predict on test, to compare with the restProbParsets fkt
    pars = list(x = oneclass.test[, -oneclass.col])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    p = do.call(dbscan::lof, pars)
    old.probs.list[[i]] = convertingScoresToProbability(as.matrix(p))$probability[, 1]
  }

  testProbParsets("oneclass.lof", oneclass.df,
    oneclass.target, oneclass.train.inds, old.probs.list, parset.list, oneclass.positive, oneclass.negative)
})

test_that("class names are integers and response predicted", {
  df = data.frame(matrix(runif(100, 0, 1), 100, 9))
  classx = factor(sample(c(0, 1), 100, replace = TRUE))
  df = cbind(classx, df)

  oneclass.task = makeOneClassTask(id = "example", data = df, target = "classx", positive = 1, negative = 0)
  ae.lrn  = makeLearner("oneclass.lof", predict.type = "response", k = 20)
  rdesc = makeResampleDesc("CV", iters = 2L)
  rin = makeResampleInstance(rdesc, task = oneclass.task)
  r = resample(ae.lrn, oneclass.task, rin)
  expect_false(is.null(r$pred))
})
