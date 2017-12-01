context("classif_lqa")

test_that("classif_lqa", {
  requirePackagesOrSkip("lqa", default.method = "load")

  parset.list = list(
    list(),
    list(penalty = "lasso", lambda = 0.01),
    list(penalty = "fused.lasso", lambda1 = 0.001, lambda2 = 0.01),
    list(penalty = "oscar", lambda = 0.01, oscar.c = 1),
    list(penalty = "genet", lambda = 0.01, gamma = 2, alpha = 0.5)
  )
  parset.list.lqa = list(
    list(family = binomial(), penalty = lqa::lasso(0.1)),
    list(family = binomial(), penalty = lqa::lasso(0.01)),
    list(family = binomial(), penalty = lqa::fused.lasso(c(0.001, 0.01))),
    list(family = binomial(), penalty = lqa::oscar(c(0.01, 1))),
    list(family = binomial(), penalty = lqa::genet(c(0.01, 0.5, 2)))
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list.lqa)) {
    parset = parset.list.lqa[[i]]
    x = binaryclass.train
    y = as.numeric(x[, binaryclass.class.col] == binaryclass.class.levs[1L])
    x[, binaryclass.class.col] = NULL
    pars = list(x = x, y = y)
    pars = c(pars, parset)
    m = do.call(lqa::lqa.default, pars)
    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL
    newx = cbind(1, newx)
    p = lqa::predict.lqa(m, newx)$mu.new
    p2 = ifelse(p > 0.5, binaryclass.class.levs[1L], binaryclass.class.levs[2L])
    old.predicts.list[[i]] = p2
    old.probs.list[[i]] = p
  }


  testSimpleParsets("classif.lqa", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets("classif.lqa", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.probs.list, parset.list)

})
