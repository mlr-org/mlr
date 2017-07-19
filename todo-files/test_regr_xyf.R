context("regr_xyf")

test_that("regr_xyf", {
  requirePackagesOrSkip("kohonen", default.method = "load")

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

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    pars = parset.list1[[i]]
    pars$data = as.matrix(regr.num.train[, -regr.num.class.col])
    pars$Y = regr.num.train[, regr.num.class.col]
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(kohonen::xyf, pars)
    p = predict(m, as.matrix(regr.num.test[, -regr.num.class.col]))
    old.predicts.list[[i]] = as.vector(p$prediction)
  }

  testSimpleParsets("regr.xyf", regr.num.df, regr.num.target, regr.num.train.inds,
    old.predicts.list, parset.list2)
})
