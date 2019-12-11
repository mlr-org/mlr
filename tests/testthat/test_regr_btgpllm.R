context("regr_btgpllm")

test_that("regr_btgpllm", {
  requirePackagesOrSkip("tgp", default.method = "load")

  parset.list = list(
    list(meanfn = "linear", bprior = "bflat", corr = "expsep")
  )

  df = regr.df[, 2:5]
  col.types = vcapply(df, function(x) class(x))
  factor.ind = (col.types == "factor")
  df.num = df[, !factor.ind, drop = FALSE]
  n.num = ncol(df.num)
  df.factor = df[, factor.ind, drop = FALSE]
  df.factor = createDummyFeatures(df.factor, method = "reference")
  df = cbind(df.num, df.factor)
  inds = 1:10
  train = df[inds, ]
  test = df[-inds, ]
  y = regr.df[inds, regr.target]

  old.predicts.list = list()
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = train, Z = y, verb = 0, basemax = n.num, pred.n = FALSE)
    pars = c(pars, parset)
    m = do.call(tgp::btgpllm, pars)
    old.predicts.list[[i]] = predict(m, XX = test, pred.n = FALSE)$ZZ.km
  }
  testSimpleParsets("regr.btgpllm", regr.df[, c(2:5, 14)], regr.target, inds,
    old.predicts.list, parset.list)
})
