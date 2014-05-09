context("surv_glmboost")

test_that("surv_glmboost", {
  library(mboost)
  library(survival)
  parset.list = list(
    list(family = CoxPH()),
    list(mstop = 50, nu = 0.2, family = CoxPH()),
    list(mstop = 200, family = CoxPH()),
    list(mstop = 250, nu = 0.05,  family = CoxPH())
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = surv.formula, data = surv.train, family = parset$family)
    ctrl = boost_control()
    if (!is.null(parset$mstop))
      ctrl$mstop = parset$mstop
    if (!is.null(parset$nu))
      ctrl$nu = parset$nu
    pars = c(pars, list(control= ctrl))
    set.seed(getOption("mlr.debug.seed"))
    # glmboost(formula = pars$formula, data = pars$data, family = CoxPH()) doesn't work, but
    # glmboost(pars$formula, data = pars$data, family = CoxPH()) does..!? Use hack for this:
    glm = function(...) glmboost(pars$formula, ...)
    # m = do.call(glmboost, pars)
    m = do.call(glm, pars[-1])

    p  = predict(m, newdata = surv.test, type = "link")
    old.predicts.list[[i]] = drop(p)
  }

  testSimpleParsets("surv.glmboost", surv.df, surv.target, surv.train.inds, old.predicts.list, parset.list)
})
