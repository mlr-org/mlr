context("surv_randomForestSRC")

test_that("surv_randomForestSRC", {
  requirePackagesOrSkip(c("survival", "randomForestSRC"),
    default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 10L),
    list(ntree = 10L, mtry = 5L),
    list(ntree = 10L, nodesize = 2L, na.action = "na.impute",
      splitrule = "logrank", importance = "permute", proximity = FALSE)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = surv.train, formula = surv.formula,
      forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    # There are some crazy checks in RFSRC, we have to overwrite the formula here
    parset$formula = Surv(time, status) ~ .
    m = do.call(randomForestSRC::rfsrc, parset)
    p = predict(m, newdata = surv.test, membership = FALSE,
      na.action = "na.impute")$predicted
    old.predicts.list[[i]] = drop(p)
  }

  testSimpleParsets("surv.randomForestSRC", surv.df, surv.target,
    surv.train.inds, old.predicts.list, parset.list)
})
