context("classif_penalized")

test_that("classif_penalized", {
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
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    capture.output({
      m = do.call(penalized::penalized, pars)
    })
    # FIXME: should be removed, reported in issue 840
    m@formula$unpenalized[[2L]] = as.symbol(binaryclass.target)
    old.probs.list[[i]] = 1 - penalized::predict(m, data = binaryclass.test)
  }
  testProbParsets("classif.penalized", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
  parset.list = list(
    list(maxiter = 100),
    list(lambda1 = 2, lambda2 = 1),
    list(lambda1 = 1, lambda2 = 2),
    list(lambda1 = 2, lambda2 = 1, maxiter = 2L, fusedl = TRUE),
    list(lambda1 = 1, lambda2 = 2, maxiter = 4L, fusedl = TRUE)
  )

  tt = function(formula, data, subset = seq_len(nrow(data)), ...) {
    penalized::penalized(formula, data = data[subset, ], ...)
  }

  tp = function(model, newdata, ...) {
    pred = penalized::predict(model, data = newdata, ...)
    ifelse(pred > 0.5, binaryclass.class.levs[2L], binaryclass.class.levs[1L])
  }

  testCVParsets("classif.penalized", binaryclass.df, binaryclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})
