context("classif_bdk")

test_that("classif_bdk", {
  requirePackages("kohonen", default.method = "load")
  parset.list1 = list(
    list(),
    list(grid = class::somgrid(xdim = 2L, ydim = 4L)),
    list(rlen = 50L)
  )
  parset.list2 = list(
    list(),
    list(xdim = 2L, ydim = 4L),
    list(rlen = 50L)
  )

  old.probs.list = old.predicts.list = list()

  for (i in 1:length(parset.list1)) {
    pars = parset.list1[[i]]
    pars$data = as.matrix(binaryclass.train[, -binaryclass.class.col])
    pars$Y = binaryclass.train[, binaryclass.class.col]
    pars$keep.data = FALSE
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(kohonen::bdk, pars)
    p = predict(m, as.matrix(binaryclass.test[, -binaryclass.class.col]))
    old.predicts.list[[i]] = p$prediction
    old.probs.list[[i]] = p$unit.predictions[p$unit.classif, 1L]
  }

  testSimpleParsets("classif.bdk", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list2)
  testProbParsets ("classif.bdk", binaryclass.df, binaryclass.target, binaryclass.train.inds,
                   old.probs.list, parset.list2)
})
