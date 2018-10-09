context("surv_kaplanmeier")

test_that("surv_coxph", {
  requirePackagesOrSkip("survival", default.method = "load")

  parset.list = list(
    list()
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    set.seed(getOption("mlr.debug.seed"))
    p = rep(median(surv.df[surv.train.inds, surv.target[1]]), nrow(surv.test))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("surv.kaplanmeier", surv.df, surv.target, surv.train.inds, old.predicts.list, parset.list)
})
