context("classif_h2ogbm")

test_that("classif_h2ogbm", {
  skip_on_travis()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(ntrees = 5L),
    list(ntrees = 5L, min_rows = 5L),
    list(ntrees = 5L, nbins = 2L)
  )
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(binaryclass.train[, -binaryclass.class.col]),
      y = binaryclass.target,
      training_frame = h2o::as.h2o(binaryclass.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.gbm, parset)
    p  = predict(m, newdata = h2o::as.h2o(binaryclass.test))
    old.probs.list[[i]] = as.data.frame(p)[, 2L]
  }

  testProbParsets("classif.h2o.gbm", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})
