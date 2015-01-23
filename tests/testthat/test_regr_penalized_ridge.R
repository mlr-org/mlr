context("regr_penalized_ridge")

test_that("regr_penalized_ridge", {
  requirePackages("!penalized", default.method = "load")
  parset.list = list(
    list(),
    list(lambda2 = 0.3),
    list(lambda2 = 1),
    list(lambda2 = 2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data=regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(penalized::penalized, pars)
      )
    set.seed(getOption("mlr.debug.seed"))
    p = penalized::predict(m, data=regr.test)
    old.predicts.list[[i]] = p[,"mu"]
  }

  testSimpleParsets("regr.penalized.ridge", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)

  #extra cv test
  folds=5
  cvl.res = cvl(regr.formula, data=regr.df, lambda2=0.3, fold=folds)
  res = makeResampleInstance(makeResampleDesc("CV", iters=folds), task=regr.task)
  for (i in 1:folds) {
    res$train.inds[[i]] = setdiff(1:nrow(regr.df), which(cvl.res$fold == i))
    res$test.inds[[i]] = which(cvl.res$fold == i)
  }
  wl = makeLearner("regr.penalized.ridge", lambda2=0.3)
  r = resample(wl, regr.task, res)
  p = as.data.frame(r$pred)
  for (i in 1:folds) {
    test.i = res$test.inds[[i]]
    rf.p = subset(p, subset=(iter==i), select="response", drop=TRUE)
    expect_equal(rf.p, cvl.res$predictions[test.i])
  }
})
