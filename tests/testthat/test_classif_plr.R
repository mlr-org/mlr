context("classif_plr")

test_that("classif_plr", {
  requirePackagesOrSkip("stepPlr", default.method = "load")

  parset.list1 = list(
    list(),
    list(cp = 0.005),
    list(cp = "aic"),
    list(cp = "bic", lambda = 1e-2)
  )
  parset.list2 = list(
    list(),
    list(cp = 0.005),
    list(cp.type = "aic"),
    list(cp.type = "bic", lambda = 1e-2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    x = binaryclass.train
    y = as.numeric(x[, binaryclass.class.col] == binaryclass.class.levs[1L])
    x[, binaryclass.class.col] = NULL
    pars = list(x = x, y = y)
    pars = c(pars, parset)
    m = do.call(stepPlr::plr, pars)
    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL
    p = stepPlr::predict.plr(m, newx = newx, type = "class")
    p = ifelse(p == 1, binaryclass.class.levs[1L], binaryclass.class.levs[2L])
    p2 = stepPlr::predict.plr(m, newx = newx, type = "response")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.plr", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.plr", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list2)
})
