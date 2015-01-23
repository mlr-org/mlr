context("regr_crs")

test_that("regr_crs", {
  requirePackages("crs", default.method = "load")
  parset.list = list(
    list(nmulti = 1, cv = "none"),
    list(degree = rep(3, 12), nmulti = 1, cv = "none"),
    list(segments = rep(3, 12), nmulti = 1, cv = "none")
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    suppressWarnings(m <- do.call(crs::crs, pars))
    set.seed(getOption("mlr.debug.seed"))
    pred = predict(m, newdata = regr.test)
    attr(pred, "lwr") = NULL
    attr(pred, "upr") = NULL
    old.predicts.list[[i]] = pred
  }

  suppressWarnings(testSimpleParsets("regr.crs", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list))
})
