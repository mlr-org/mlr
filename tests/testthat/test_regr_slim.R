context("regr_slim")

test_that("regr_slim", {
  requirePackagesOrSkip("flare", default.method = "load")

  parset.list = list(
    list(),
    list(method = "dantzig"),
    list(method = "lasso", nlambda = 10L, lambda.idx = 5L)
  )

  old.predicts.list = list()
  ind = setdiff(names(regr.num.train), regr.num.target)
  X = regr.num.train[, ind]
  y = regr.num.train[, regr.num.target]

  for (i in seq_along(parset.list)) {
    pars = list(X = as.matrix(X), Y = y)
    pars = c(pars, parset.list[[i]])
    if ("lambda.idx" %in% names(pars)) {
      idx = pars$lambda.idx
      pars$lambda.idx = NULL
    } else {
      idx = 3L
    }
    capture.output({
      m = do.call(flare::slim, pars)
      p = predict(m, newdata = as.matrix(regr.num.test[, ind]),
        lambda.idx = idx)[[1L]][, 1L]
    })
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.slim", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list)
})
