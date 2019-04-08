context("classif_parallelForest")

test_that("classif_parallelForest", {
  requirePackages("ParallelForest", default.method = "load")

  parset.list = list(
    list(),
    list(numboots = 5L, numvars = 2L),
    list(numboots = 10L, numsamps = 5L)
    )

  #parallelForest ist not reproducible with set.seed, so we just check for createability
  for(i in seq_along(parset.list)){
    parset = parset.list[[i]]
    pf.classif.lrn = try(makeLearner("classif.parallelForest", par.vals = parset, predict.type = "response"))
    expect_is(pf.classif.lrn, "classif.parallelForest")
    pf.classif.m = try(train(pf.classif.lrn, binaryclass.task))
    #expect_is(pf.classif.m, "WrappedModel")
    pf.classif.p = try(predict(pf.classif.m, newdata = binaryclass.test))
    expect_is(pf.classif.p, c("PredictionClassif", "Prediction"))
  }
}) 