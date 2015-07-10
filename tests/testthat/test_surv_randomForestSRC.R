context("surv_randomForestSRC")

test_that("surv_randomForestSRC", {
  requirePackages("survival", default.method = "load")
  requirePackages("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 100),
    list(ntree = 50, mtry = 4),
    list(ntree = 50, nodesize = 2, na.action = "na.impute", splitrule = "logrank")
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = surv.train, formula = surv.formula, importance = "none", proximity = FALSE, forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    # There are some crazy checks in RFSRC, we have to overwrite the formula here
    parset$formula = Surv(time, event) ~ .
    m = do.call(randomForestSRC::rfsrc, parset)
    p  = predict(m, newdata = surv.test, importance = "none", na.action = "na.impute")$predicted
    old.predicts.list[[i]] = drop(p)
  }

  testSimpleParsets("surv.randomForestSRC", surv.df, surv.target, surv.train.inds, old.predicts.list, parset.list)
})
