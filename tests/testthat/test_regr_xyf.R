context("regr_xyf")

test_that("regr_xyf", {
  requirePackagesOrSkip("kohonen", default.method = "load")

  parset.list1 = list(
    list(),
    list(grid = kohonen::somgrid(xdim = 2L, ydim = 4L)),
    list(rlen = 50L)
  )
  parset.list2 = list(
    list(),
    list(xdim = 2L, ydim = 4L),
    list(rlen = 50L)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list1)) {
    pars = parset.list1[[i]]
    pars$data = list()
    pars$data$X = as.matrix(regr.num.train[, -regr.num.class.col])
    pars$data$Y = as.matrix(regr.num.train[, regr.num.class.col, drop = FALSE])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(kohonen::supersom, pars)
    p = predict(m, list(X = as.matrix(regr.num.test[, -regr.num.class.col])))
    old.predicts.list[[i]] = as.vector(p$predictions[[2]])
  }

  testSimpleParsets("regr.xyf", regr.num.df, regr.num.target, regr.num.train.inds,
    old.predicts.list, parset.list2)
})
