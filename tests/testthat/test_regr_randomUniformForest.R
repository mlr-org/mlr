context("regr_randomUniformForest")

test_that("regr_randomUniformForest", {
  requirePackages("randomUniformForest", default.method = "load")

  parset.list = list(
    list(),
    list(ntree=50, mtry=2)
    )
  #randomUniformForest ist not reproducible with set.seed, so we just check for createability
  for(i in 1:length(parset.list)){
     parset = parset.list[[i]]
    ruf.regr.lrn = try(makeLearner("regr.randomUniformForest", par.vals = parset))
    expect_is(ruf.regr.lrn, "regr.randomUniformForest")
    ruf.regr.m = try(train(ruf.regr.lrn, regr.task))
    expect_is(ruf.regr.m, "WrappedModel")
    ruf.regr.p = try(predict(ruf.regr.m, newdata = regr.test))
    expect_is(ruf.regr.p, c("PredictionRegr", "Prediction"))
  }
})