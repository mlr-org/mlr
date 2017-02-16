context("surv_penalized_ridge")

test_that("surv_penalized_ridge", {
  requirePackages("survival", default.method = "load")
  requirePackages("penalized", default.method = "load")
  parset.list = list(
    list(),
    list(lambda2 = 1),
    list(lambda2 = 2, maxiter = 15L)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    pars = c(list(response = surv.formula, data = surv.train,
      trace = FALSE, model = "cox", fused = FALSE), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(penalized::penalized, pars)
    p = penalized::survival(penalized::predict(m, data = surv.test), Inf)
    old.predicts.list[[i]] = p
  }
  testSimpleParsets("surv.penalized.ridge", surv.df, surv.target,
    surv.train.inds, old.predicts.list, parset.list)
})
