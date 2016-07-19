context("classif_bst")

test_that("classif_bst", {
  requirePackagesOrSkip("bst", default.method = "load")

  parset.list1 = list(
    list(control.tree = list(xval = 0L)),
    list(cost = 0.6, family = "gaussian", control.tree = list(xval = 0L)),
    list(ctrl = bst::bst_control(mstop = 40L), control.tree = list(xval = 0L)),
    list(learner = "tree", control.tree = list(maxdepth = 2L, xval = 0L))
  )

  parset.list2 = list(
    list(xval = 0L),
    list(cost = 0.6, family = "gaussian", xval = 0L),
    list(mstop = 40L, xval = 0L),
    list(Learner = "tree", maxdepth = 2L, xval = 0L)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    parset$y = ifelse(binaryclass.train[, binaryclass.class.col] == binaryclass.class.levs[2], 1, -1)
    parset$x = binaryclass.train[, -binaryclass.class.col]
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(bst::bst, parset)
    p = predict(m, binaryclass.test)
    old.predicts.list[[i]] = ifelse(p > 0, binaryclass.class.levs[2], binaryclass.class.levs[1])
  }

  testSimpleParsets("classif.bst", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list2)
})
