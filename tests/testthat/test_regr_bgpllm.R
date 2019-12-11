context("regr_bgpllm")

test_that("regr_bgpllm", {
  requirePackagesOrSkip("tgp", default.method = "load")

  parset.list = list(
    list(meanfn = "linear", bprior = "bflat", corr = "expsep")
  )
  inds = 1:50
  y = regr.num.df[inds, regr.num.target]
  old.predicts.list = list()
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = regr.num.df[inds, -regr.num.class.col], Z = y,
      verb = 0, pred.n = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(tgp::bgpllm, pars)

    old.predicts.list[[i]] = predict(m,
      XX = regr.num.df[-inds, -regr.num.class.col], pred.n = FALSE)$ZZ.km
  }
  testSimpleParsets("regr.bgpllm", regr.num.df, regr.num.target, inds,
    old.predicts.list, parset.list)
})
