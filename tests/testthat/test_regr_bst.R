context("regr_bst")

test_that("classif_bst", {
  requirePackagesOrSkip("bst", default.method = "load")

  parset.list1 = list(
    list(),
    list(cost = 0.6),
    list(ctrl = bst::bst_control(mstop = 10L)),
    list(learner = "tree", control.tree = list(maxdepth = 2L))
  )

  parset.list2 = list(
    list(),
    list(cost = 0.6),
    list(mstop = 10L),
    list(Learner = "tree", maxdepth = 2L)
  )

  old.predicts.list = list()
  xind = names(regr.num.train) != regr.num.target

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    parset$y = regr.num.train[, regr.num.target]
    parset$x = regr.num.train[, xind]
    parset$family = "gaussian"
    # set.seed(getOption("mlr.debug.seed"))
    m = do.call(bst::bst, parset)
    old.predicts.list[[i]] = predict(m, regr.num.test[, xind])
  }
  testSimpleParsets("regr.bst", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list2)
})
