context("classif_gausspr")

test_that("classif_gausspr", {
  requirePackages("kernlab", default.method = "load")

  parset.list = list(
    list(),
    list(kernel = "splinedot"),
    list(tol = 0.2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(kernlab::gausspr, pars)
    p = kernlab::predict(m, newdata = multiclass.test[, -5], type = "response")
    p2 = kernlab::predict(m, newdata = multiclass.test[, -5], type = "probabilities")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.gausspr", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.gausspr", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)
})
