context("surv_penalized")

test_that("surv_penalized", {
  requirePackages("survival", default.method = "load")
  requirePackages("penalized", default.method = "load")
  parset.list = list(
    list(maxiter = 100),
    list(lambda1 = 2, lambda2 = 1),
    list(lambda1 = 1, lambda2 = 1),
    list(fusedl = TRUE, lambda1 = 2, lambda2 = 1, maxiter = 5L),
    list(fusedl = TRUE, lambda1 = 1, lambda2 = 1, maxiter = 10L)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    pars = c(list(response = surv.formula, data = surv.train,
      model = "cox"), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(penalized::penalized, pars)
    p = penalized::survival(penalized::predict(m, data = surv.test), Inf)
    old.predicts.list[[i]] = p
  }
  testSimpleParsets("surv.penalized", surv.df, surv.target,
    surv.train.inds, old.predicts.list, parset.list)
})
