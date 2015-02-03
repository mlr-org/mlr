context("regr_bgpllm")

test_that("regr_bgpllm", {
  requirePackages("tgp", default.method = "load")
  parset.list = list(
    list(meanfn = "linear", bprior = "bflat", corr = "expsep")
  )
  inds = 1:10
  y = regr.df[inds, regr.target]
  old.predicts.list = list()
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = regr.df[inds, 1:3], Z = y, verb = 0, pred.n = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(tgp::bgpllm, pars)

    old.predicts.list[[i]] = predict(m, XX = regr.df[-inds, 1:3], pred.n = FALSE)$ZZ.km
  }
  testSimpleParsets("regr.bgpllm", regr.df[, c(1:3, 14)], regr.target, inds, old.predicts.list, parset.list)
})
