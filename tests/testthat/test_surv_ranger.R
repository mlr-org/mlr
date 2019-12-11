context("surv_ranger")

## FIXME: Proper test required when predictions working
test_that("surv_ranger", {
  requirePackagesOrSkip(c("survival", "ranger"), default.method = "load")

  lrn = makeLearner("surv.ranger")
  task = makeSurvTask(data = surv.train, target = surv.target)

  m = mlr::train(lrn, task)
  expect_equal(m$learner.model$treetype, "Survival")
  p = predict(m, newdata = surv.test)
  expect_is(p, "PredictionSurv")

  parset.list = list(
    list(),
    list(num.trees = 10L)
  )

  old.predicts.list = list()
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = surv.formula, data = surv.train,
      respect.unordered.factors = TRUE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, pars)
    p = predict(m, data = surv.test)
    old.predicts.list[[i]] = rowMeans(p$chf)
  }

  testSimpleParsets("surv.ranger", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list)
})
