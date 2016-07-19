context("surv_penalized_fusedlasso")

test_that("surv_penalized_fusedlasso", {
  requirePackages("survival", default.method = "load")
  requirePackages("penalized", default.method = "load")
  parset.list = list(
    list(),
    list(lambda1 = 2, lambda2 = 1, maxiter = 5L),
    list(lambda1 = 1, lambda2 = 1, maxiter = 10L)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    if (is.null(parset.list[[i]]$lambda1))
      parset.list[[i]]$lambda1 = 1
    if (is.null(parset.list[[i]]$lambda2))
      parset.list[[i]]$lambda2 = 1
    pars = c(list(response = surv.formula, data = surv.train,
      model = "cox", trace = FALSE, fusedl = TRUE), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(penalized::penalized, pars)
    p = penalized::survival(penalized::predict(m, data = surv.test), Inf)
    old.predicts.list[[i]] = p
  }
  testSimpleParsets("surv.penalized.fusedlasso", surv.df, surv.target,
    surv.train.inds, old.predicts.list, parset.list)
})
