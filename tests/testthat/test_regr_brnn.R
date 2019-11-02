context("regr_brnn")

test_that("regr_brnn", {
  requirePackagesOrSkip("brnn", default.method = "load")

  parset.list = list(
    list(),
    list(neurons = 3L),
    list(mu = 0.001)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(brnn::brnn, pars)
    })
    p = predict(m, newdata = regr.test)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.brnn", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
