context("regr_frbs")

test_that("regr_frbs", {
  requirePackagesOrSkip("frbs", default.method = "load")

  parset.list = list(
    # list(),
    list(num.labels = 2L, type.mf = "TRAPEZOID")
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    pars = parset.list[[i]]
    method.arg = names(pars) == "method"
    if (any(method.arg)) {
      pars = list(method = pars$method, control = pars[!method.arg])
    } else {
      pars = list(control = pars)
    }
    pars$data.train = regr.num.train
    m = do.call(frbs::frbs.learn, pars)
    ind = setdiff(names(regr.num.test), regr.num.target)
    p = suppressWarnings(predict(m, newdata = regr.num.test[, ind])[, 1])
    old.predicts.list[[i]] = p
  }

  # suppressed warnings:a
  # "There are your newdata which are out of the specified range"
  suppressWarnings(
    testSimpleParsets("regr.frbs", regr.num.df, regr.num.target,
      regr.num.train.inds, old.predicts.list, parset.list)
  )
})
