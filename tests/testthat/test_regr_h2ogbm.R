context("regr_h2ogbm")

test_that("regr_h2ogbm", {
  skip_on_ci()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(ntrees = 5L),
    list(ntrees = 5L, min_rows = 5L),
    list(ntrees = 5L, nbins = 2L),
    list(ntrees = 5L, max_depth = 5L),
    list(ntrees = 5L, nbins = 5L),
    list(ntrees = 5L, nbins_cats = 512L),
    list(ntrees = 5L, nbins_top_level = 2048L),
    list(ntrees = 5L, learn_rate = 0.2),
    list(ntrees = 5L, learn_rate_annealing = 0.99),
    list(ntrees = 5L, distribution = "poisson"),
    list(ntrees = 5L, sample_rate = 0.5, seed = getOption("mlr.debug.seed")),
    list(ntrees = 5L, col_sample_rate = 0.5, seed = getOption("mlr.debug.seed")),
    list(ntrees = 5L, col_sample_rate_change_per_level = 0.9, seed = getOption("mlr.debug.seed")),
    list(ntrees = 5L, col_sample_rate_per_tree = 0.5, seed = getOption("mlr.debug.seed")),
    list(ntrees = 5L, max_abs_leafnode_pred = 200),
    list(ntrees = 5L, pred_noise_bandwidth = 0.2, seed = getOption("mlr.debug.seed")),
    list(ntrees = 5L, categorical_encoding = "OneHotExplicit"),
    list(ntrees = 5L, min_split_improvement = 1e-4),
    list(ntrees = 5L, histogram_type = "Random", seed = getOption("mlr.debug.seed")),
    list(ntrees = 5L, score_each_iteration = TRUE),
    list(ntrees = 5L, score_tree_interval = 2),
    list(ntrees = 5L, stopping_rounds = 3, stopping_metric = "MAE"),
    list(ntrees = 5L, distribution = "quantile", quantile_alpha = 0.6),
    list(ntrees = 5L, distribution = "tweedie", tweedie_power = 1.1),
    list(ntrees = 5L, distribution = "huber", huber_alpha = 0.8),
    list(ntrees = 5, seed = 1)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(regr.train[, -regr.class.col]),
      y = regr.target,
      training_frame = h2o::as.h2o(regr.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.gbm, parset)
    p = predict(m, newdata = h2o::as.h2o(regr.test))
    old.predicts.list[[i]] = as.data.frame(p)[, 1L]
  }

  testSimpleParsets("regr.h2o.gbm", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
