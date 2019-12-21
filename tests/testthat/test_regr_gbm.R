context("regr_gbm")

test_that("regr_gbm", {
  requirePackagesOrSkip("gbm", default.method = "load")

  parset.list = list(
    list(),
    list(n.trees = 10, distribution = "gaussian"),
    list(interaction.depth = 2, distribution = "gaussian"),
    list(distribution = list(name = "quantile", alpha = 0.2))
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(gbm::gbm, pars)
    })
    p = gbm::predict.gbm(m, newdata = regr.test, n.trees = length(m$trees))
    old.predicts.list[[i]] = p
  }

  #  Different way to pass quantile distribution in mlr
  parset.list[[4]]$distribution = "quantile"
  parset.list[[4]]$alpha = 0.2

  testSimpleParsets("regr.gbm", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})

test_that("regr_gbm keep.data is passed correctly", {
  expect_silent(train(makeLearner("regr.gbm", keep.data = FALSE), regr.task))
  expect_silent(train(makeLearner("regr.gbm", keep.data = TRUE), regr.task))
})
