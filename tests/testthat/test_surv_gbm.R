context("surv_gbm")

test_that("surv_gbm", {
  requirePackagesOrSkip("gbm", default.method = "load")

  parset.list = list(
    list(),
    list(n.trees = 10L),
    list(interaction.depth = 2L, n.trees = 10L)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(surv.formula, data = surv.train, distribution = "coxph")
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(gbm::gbm, pars)
    })
    p = gbm::predict.gbm(m, newdata = surv.test, n.trees = m$n.trees)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("surv.gbm", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list)
})
