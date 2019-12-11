context("surv_glmboost")

test_that("surv_glmboost", {
  requirePackagesOrSkip(c("survival", "mboost"), default.method = "load")

  parset.list1 = list(
    list(family = mboost::CoxPH()),
    list(family = mboost::CoxPH(),
      control = mboost::boost_control(mstop = 100L, nu = 0.1)),
    list(family = mboost::Weibull(nuirange = c(0, 50.5)),
      control = mboost::boost_control(mstop = 50L, nu = 1)),
    list(family = mboost::Gehan(),
      control = mboost::boost_control(mstop = 100L, nu = 0.5))
  )

  parset.list2 = list(
    list(),
    list(mstop = 100L, nu = 0.1),
    list(family = "Weibull", nuirange = c(0, 50.5), mstop = 50L, nu = 1),
    list(family = "Gehan", mstop = 100L, nu = 0.5)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    f = getTaskFormula(surv.task)
    pars = list(f, data = surv.train)
    pars = c(pars, parset)
    # suppressed warnings: "NA/Inf replaced by maximum positive value"
    m = suppressWarnings(do.call(mboost::glmboost, pars))
    # suppressed warnings: "NA/Inf replaced by maximum positive value"
    p = suppressWarnings(predict(m, newdata = surv.test, type = "link"))
    old.predicts.list[[i]] = drop(p)
  }

  # suppressed warnings: "NA/Inf replaced by maximum positive value"
  suppressWarnings(
    testSimpleParsets("surv.glmboost", surv.df, surv.target, surv.train.inds,
      old.predicts.list, parset.list2)
  )

  # test alternative matrix interface
  mod1 = train(makeLearner("surv.glmboost", use.formula = FALSE,
    center = FALSE), wpbc.task)
  mod2 = train(makeLearner("surv.glmboost", use.formula = TRUE,
    center = FALSE), wpbc.task)
  expect_equal(coef(mod1$learner.model), coef(mod2$learner.model))
})
