context("regr_h2odeeplearning")

test_that("regr_h2odeeplearning", {
  skip_on_ci()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(distribution = "gaussian"),
    list(distribution = "quantile", quantile_alpha = 0.2),
    list(distribution = "tweedie", tweedie_power = 1.2)
  )
  # h20deeplearning needs seed in function call to be reproducible
  debug.seed = getOption("mlr.debug.seed")
  parset.list = lapply(parset.list, function(x) {
    c(x, seed = debug.seed,
      reproducible = TRUE)
  })
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(regr.train[, -regr.class.col]),
      y = regr.target,
      training_frame = h2o::as.h2o(regr.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.deeplearning, parset)
    p = predict(m, newdata = h2o::as.h2o(regr.test))
    old.predicts.list[[i]] = as.data.frame(p)[, 1L]
  }

  testSimpleParsets("regr.h2o.deeplearning", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)
})
