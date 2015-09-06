context("classif_randomUniformForest")

test_that("classif_randomUniformForest", {
  requirePackages("randomUniformForest", default.method = "load")

  parset.list = list(
    list(),
    list(ntree=50, mtry=2)
    )
  type = c("response", "prob")

  #randomUniformForest ist not reproducible with set.seed, so we just check for createability
  for(i in 1:length(parset.list)){
    for(j in 1:length(type)){
          parset = parset.list[[i]]
    ruf.classif.lrn = try(makeLearner("classif.randomUniformForest", par.vals = parset, predict.type = type[j]))
    expect_is(ruf.classif.lrn, "classif.randomUniformForest")
    ruf.classif.m = try(train(ruf.classif.lrn, multiclass.task))
    expect_is(ruf.classif.m, "WrappedModel")
    ruf.classif.p = try(predict(ruf.classif.m, newdata = multiclass.test))
    expect_is(ruf.classif.p, c("PredictionClassif", "Prediction"))

    }
  }
})