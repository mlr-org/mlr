context("regr_btlm")

test_that("regr_btlm", {
  requirePackagesOrSkip("tgp", default.method = "load")

  parset.list = list(
    list(),
    list(meanfn = "linear", bprior = "bflat"),
    list(meanfn = "linear", tree = c(0.5, 3)),
    list(meanfn = "constant")
  )

  y = regr.train[, regr.target]

  df = regr.df[, 2:5]
  col.types = vcapply(df, function(x) class(x))
  factor.ind = (col.types == "factor")
  df.num = df[, !factor.ind, drop = FALSE]
  n.num = ncol(df.num)
  df.factor = df[, factor.ind, drop = FALSE]
  df.factor = createDummyFeatures(df.factor, method = "reference")
  df = cbind(df.num, df.factor)
  train = df[regr.train.inds, ]
  test = df[regr.test.inds, ]

  old.predicts.list = list()
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(X = train, Z = y, verb = 0, basemax = n.num, pred.n = FALSE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(tgp::btlm, pars)

    old.predicts.list[[i]] = predict(m, XX = test, pred.n = FALSE)$ZZ.km
  }
  testSimpleParsets("regr.btlm", regr.df[, c(2:5, 14)], regr.target,
    regr.train.inds, old.predicts.list, parset.list)
})
