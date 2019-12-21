context("regr_nodeHarvest")

test_that("regr_nodeHarvest", {
  requirePackagesOrSkip("nodeHarvest", default.method = "load")

  parset.list = list(
    list(nodes = 10L),
    list(nodes = 10L, maxinter = 1L),
    list(nodes = 10L, mode = "outbag")
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(X = regr.df[regr.train.inds, -regr.class.col],
      Y = regr.df[regr.train.inds, regr.class.col], silent = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(nodeHarvest::nodeHarvest, parset)
    old.predicts.list[[i]] = predict(m, regr.df[-regr.train.inds, ])
  }

  testSimpleParsets("regr.nodeHarvest", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
