context("surv_penalized_lasso")

test_that("surv_penalized_lasso", {
  requirePackages("survival", default.method = "load")
  requirePackages("penalized", default.method = "load")
  parset.list = list(
    list(),
    list(lambda1 = 1),
    list(lambda1 = 2)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    pars = c(list(response = surv.formula, data = surv.train,
      model = "cox", trace = FALSE), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(penalized::penalized, pars)
    p = penalized::survival(penalized::predict(m, data = surv.test), Inf)
    old.predicts.list[[i]] = p
  }
  testSimpleParsets("surv.penalized.lasso", surv.df, surv.target,
    surv.train.inds, old.predicts.list, parset.list)
})
