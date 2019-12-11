context("surv_cv.CoxBoost")

test_that("surv_cv.CoxBoost", {
  requirePackagesOrSkip(c("!Matrix", "!CoxBoost"), default.method = "load")

  parset.list = list(
    list(),
    list(penalty = 5, maxstepno = 200)
  )
  old.predicts.list = list()

  # i = 1
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    y = as.matrix(surv.train[, surv.target])
    colnames(y) = c("time", "status")
    x = dropNamed(surv.train, surv.target)

    info = getFixDataInfo(surv.train, factors.to.dummies = TRUE,
      ordered.to.int = TRUE)
    pars = c(list(time = unname(y[, surv.target[1]]),
      status = unname(y[, surv.target[2]]), x = as.matrix(fixDataForLearner(x,
        info))), parset)
    if (is.null(pars$penalty)) {
      pars$penalty = penalty = 9 * sum(unname(y[, surv.target[2]]))
    }
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(CoxBoost::cv.CoxBoost, pars)

    pars$stepno = m$optimal.step
    pars$K = NULL
    pars$maxstepno = NULL
    m = do.call(CoxBoost::CoxBoost, pars)
    p = as.numeric(predict(m,
      newdata = as.matrix(fixDataForLearner(dropNamed(surv.test, surv.target),
        info)), type = "lp"))
    old.predicts.list[[i]] = as.numeric(p)
  }

  testSimpleParsets("surv.cv.CoxBoost", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list)

  if (FALSE) {
    lrn = makeLearner("surv.cv.CoxBoost")
    m = train(lrn, task = surv.task, subset = surv.train.inds)
    p = predict(m, task = surv.task)
  }
})
