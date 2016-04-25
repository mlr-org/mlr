context("regr_penalized_lasso")

test_that("regr_penalized_lasso", {
  requirePackagesOrSkip("!penalized", default.method = "load")

  parset.list = list(
    list(),
    list(lambda1 = 0.3),
    list(lambda1 = 1),
    list(lambda1 = 2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train, trace = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(penalized::penalized, pars)
    )
    # FIXME: should be removed, reported in issue 840
    m@formula$unpenalized[[2L]] = as.symbol(regr.target)
    p = penalized::predict(m, data = regr.test)
    old.predicts.list[[i]] = p[,"mu"]
  }
  testSimpleParsets("regr.penalized.lasso", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)

  tt = function(formula, data, subset = 1:nrow(data), ...) {
    penalized::penalized(formula, data = data[subset, ], fusedl = FALSE, ...)
  }

  tp = function(model, newdata, ...) {
    # FIXME: should be removed, reported in issue 840
    model@formula$unpenalized[[2L]] = as.symbol(regr.target)
    penalized::predict(model, data = newdata,...)[, "mu"]
  }

  testCVParsets("regr.penalized.lasso", regr.df, regr.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
