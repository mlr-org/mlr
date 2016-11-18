context("surv_ranger")

## TODO: Proper test required when predictions working
## Suppress 'experimental' warning for splirule = maxstat 
test_that("surv_ranger", {
  requirePackagesOrSkip(c("survival", "ranger"), default.method = "load")

  lrn = makeLearner("surv.ranger")
  task = makeSurvTask(data = surv.train, target = surv.target)

  set.seed(getOption("mlr.debug.seed"))
  m = suppressWarnings(mlr::train(lrn, task))
  expect_equal(m$learner.model$treetype, "Survival")
  p = predict(m, newdata = surv.test)
  expect_is(p, "PredictionSurv")

  parset.list = list(
    list(),
    list(num.trees = 10L),
    list(num.trees = 10L, splitrule = "maxstat", alpha = 0.25),
    list(num.trees = 10L, splitrule = "maxstat", minprop = 0.25)
  )

  old.predicts.list = list()
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = surv.formula, data = surv.train, respect.unordered.factors = TRUE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = suppressWarnings(do.call(ranger::ranger, pars))
    p = predict(m, data = surv.test)
    old.predicts.list[[i]] = rowMeans(p$chf)
  }

  suppressWarnings(testSimpleParsets("surv.ranger", surv.df, surv.target, surv.train.inds, old.predicts.list, parset.list))
})
