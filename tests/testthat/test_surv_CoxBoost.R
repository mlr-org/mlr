context("surv_CoxBoost")

test_that("surv_CoxBoost", {
  requirePackagesOrSkip(c("!Matrix", "!CoxBoost"), default.method = "load")

  parset.list = list(
    list(),
    list(stepno = 10)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    y = as.matrix(surv.train[, surv.target])
    colnames(y) = c("time", "status")
    x = dropNamed(surv.train, surv.target)

    penalty = 9 * sum(y[, "status"])
    info = getFixDataInfo(surv.train, factors.to.dummies = TRUE,
      ordered.to.int = TRUE)
    pars = c(list(time = unname(y[, "time"]), status = unname(y[, "status"]),
      return.score = FALSE, penalty = penalty,
      x = as.matrix(fixDataForLearner(x, info))), parset)
    m = do.call(CoxBoost::CoxBoost, pars)
    p = as.numeric(predict(m,
      newdata = as.matrix(fixDataForLearner(dropNamed(surv.test, surv.target),
        info)), type = "lp"))
    old.predicts.list[[i]] = as.numeric(p)
  }

  testSimpleParsets("surv.CoxBoost", surv.df, surv.target, surv.train.inds,
    old.predicts.list, parset.list)
})
