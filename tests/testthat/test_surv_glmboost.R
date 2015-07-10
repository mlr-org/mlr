context("surv_glmboost")

test_that("surv_glmboost", {
  requireNamespace("survival")
  requireNamespace("mboost")
  parset.list = list(
    list(mstop = 100L, nu = 0.1),
    list(mstop = 50L, nu = 1),
    list(mstop = 100L, nu = 0.5)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    ctrl = mboost::boost_control(mstop = parset$mstop, nu = parset$nu)
    f = getTaskFormula(surv.task)
    pars = list(f, data = surv.train, control = ctrl, family = mboost::CoxPH())
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::glmboost, pars)

    p  = predict(m, newdata = surv.test, type = "link")
    old.predicts.list[[i]] = drop(p)
  }

  testSimpleParsets("surv.glmboost", surv.df, surv.target, surv.train.inds, old.predicts.list, parset.list)

  # test alternative matrix interface
  mod1 = train(makeLearner("surv.glmboost", use.formula = FALSE, center = FALSE), wpbc.task)
  mod2 = train(makeLearner("surv.glmboost", use.formula = TRUE, center = FALSE), wpbc.task)
  expect_equal(coef(mod1$learner.model), coef(mod2$learner.model))
})
