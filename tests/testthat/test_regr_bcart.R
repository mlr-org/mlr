context("regr_bcart")

test_that("regr_bcart", {
  requirePackagesOrSkip("tgp", default.method = "load")

  parset.list = list(
    list(),
    list(bprior = "bflat"),
    list(bprior = "b0", tree = c(0.5, 3)),
    list(bprior = "bmle", tree = c(0.1, 2))
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
    m = do.call(tgp::bcart, pars)

    old.predicts.list[[i]] = predict(m, XX = test, pred.n = FALSE)$ZZ.km
  }
  testSimpleParsets("regr.bcart", regr.df[, c(2:5, 14)], regr.target,
    regr.train.inds, old.predicts.list, parset.list)
})
