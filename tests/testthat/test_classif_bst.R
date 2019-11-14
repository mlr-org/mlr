context("classif_bst")

test_that("classif_bst", {
  requirePackagesOrSkip("bst", default.method = "load")

  parset.list1 = list(
    list(),
    list(cost = 0.6, family = "gaussian"),
    list(ctrl = bst::bst_control(mstop = 40L)),
    list(learner = "tree", control.tree = list(maxdepth = 2L))
  )

  parset.list2 = list(
    list(),
    list(cost = 0.6, family = "gaussian"),
    list(mstop = 40L),
    list(Learner = "tree", maxdepth = 2L)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    parset$y = ifelse(binaryclass.train[, binaryclass.class.col] == binaryclass.class.levs[2], 1, -1)
    parset$x = binaryclass.train[, -binaryclass.class.col]
    m = do.call(bst::bst, parset)
    p = predict(m, binaryclass.test)
    old.predicts.list[[i]] = ifelse(p > 0, binaryclass.class.levs[2], binaryclass.class.levs[1])
  }

  testSimpleParsets("classif.bst", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list2)
})
