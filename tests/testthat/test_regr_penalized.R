context("regr_penalized")

test_that("regr_penalized", {
  requirePackages("!penalized", default.method = "load")

  parset.list = list(
    list(maxiter = 100),
    list(lambda1 = 2),
    list(lambda1 = 2, lambda2 = 1),
    list(lambda2 = 2),
    list(fusedl = TRUE, lambda1 = 2, maxiter = 20L),
    list(fusedl = TRUE, lambda1 = 2, lambda2 = 1, maxiter = 5L),
    list(fusedl = TRUE, lambda2 = 2, maxiter = 20L)
  )

  # to make test of empty list feasable (in terms of time), number of obs need
  # to be reduced
  regr.train.inds = sample(seq(1, 506), size = 150)
  regr.test.inds = setdiff(seq_len(nrow(regr.df)), regr.train.inds)
  regr.train = regr.df[regr.train.inds, ]
  regr.test = regr.df[regr.test.inds, ]

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    capture.output({
      m = do.call(penalized::penalized, pars)
    })
    # FIXME: should be removed, reported in issue 840
    m@formula$unpenalized[[2L]] = as.symbol(regr.target)
    p = penalized::predict(m, data = regr.test)
    old.predicts.list[[i]] = p[, "mu"]
  }
  testSimpleParsets("regr.penalized", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)

  parset.list = list(
    list(maxiter = 100),
    list(lambda1 = 2, lambda2 = 1),
    list(lambda1 = 1, lambda2 = 2),
    list(fusedl = TRUE, lambda1 = 2, lambda2 = 1, maxiter = 2L),
    list(fusedl = TRUE, lambda1 = 1, lambda2 = 2, maxiter = 4L)
  )

  tt = function(formula, data, subset = seq_len(nrow(data)), ...) {
    penalized::penalized(formula, data = data[subset, ], ...)
  }

  tp = function(model, newdata, ...) {
    penalized::predict(model, data = newdata, ...)[, "mu"]
  }

  testCVParsets("regr.penalized", regr.df, regr.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
