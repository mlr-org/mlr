context("regr_gbm")

test_that("regr_gbm", {
  requirePackages("gbm", default.method = "load")
  parset.list = list(
    list(),
    list(n.trees=600),
    list(interaction.depth = 2)
    )


  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data=regr.train, distribution="gaussian")
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(gbm::gbm, pars)
    )
    set.seed(getOption("mlr.debug.seed"))
    p = gbm::predict.gbm(m, newdata=regr.test, n.trees=length(m$trees))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.gbm", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
