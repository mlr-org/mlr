context("surv_gamboost")

test_that("surv_gamboost", {
  requirePackagesOrSkip(c("survival", "mboost"), default.method = "attach")

  parset.list1 = list(
    list(family = mboost::CoxPH()),
    list(family = mboost::CoxPH(), baselearner = "bols",
      control = mboost::boost_control(mstop = 90L, nu = 0.3)),
    list(family = mboost::Weibull(nuirange = c(0, 50.5)), baselearner = "btree",
      control = mboost::boost_control(mstop = 50L, nu = 1)),
    list(family = mboost::Gehan(), baselearner = "bbs", dfbase = 3,
      control = mboost::boost_control(mstop = 100L, nu = 0.5))
  )

  parset.list2 = list(
    list(),
    list(baselearner = "bols", mstop = 90L, nu = 0.3),
    list(family = "Weibull", baselearner = "btree", nuirange = c(0, 50.5),
      mstop = 50L, nu = 1),
    list(family = "Gehan", baselearner = "bbs", dfbase = 3, mstop = 100L,
      nu = 0.5)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    f = getTaskFormula(surv.task)
    pars = list(f, data = surv.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::gamboost, pars)
    # suppressed warnings: "Some ‘x’ values are beyond ‘boundary.knots’; Linear
    # extrapolation used."
    p = suppressWarnings(predict(m, newdata = surv.test, type = "link"))
    old.predicts.list[[i]] = drop(p)
  }

  # suppressed warnings: "Some ‘x’ values are beyond ‘boundary.knots’; Linear
  # extrapolation used."
  suppressWarnings(
    testSimpleParsets("surv.gamboost", surv.df, surv.target, surv.train.inds,
      old.predicts.list, parset.list2)
  )
})
