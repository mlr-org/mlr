context("regr_h2orandomForest")

test_that("regr_h2orandomForest", {
  skip_on_ci()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(ntrees = 4),
    list(ntrees = 4, mtries = 2)
  )
  # h2o.randomForest needs seed in function call to be reproducible
  debug.seed = getOption("mlr.debug.seed")
  parset.list = lapply(parset.list, function(x) c(x, seed = debug.seed))
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(regr.train[, -regr.class.col]),
      y = regr.target,
      training_frame = h2o::as.h2o(regr.train)))
    m = do.call(h2o::h2o.randomForest, parset)
    p = predict(m, newdata = h2o::as.h2o(regr.test))
    old.predicts.list[[i]] = as.data.frame(p)[, 1L]
  }
  testSimpleParsets("regr.h2o.randomForest", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)
})
