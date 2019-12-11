context("regr_h2oglm")

test_that("regr_h2oglm", {
  skip_on_ci()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(alpha = 1),
    list(alpha = 1, lambda = 0.2)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(regr.train[, -regr.class.col]),
      y = regr.target,
      training_frame = h2o::as.h2o(regr.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.glm, parset)
    p = predict(m, newdata = h2o::as.h2o(regr.test))
    old.predicts.list[[i]] = as.data.frame(p)[, 1L]
  }

  testSimpleParsets("regr.h2o.glm", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
