context("tuneWrapper")

test_that("tuneWrapper", {
  # cf. issue 242
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlGrid(tune.threshold = TRUE)
  ps = makeParamSet(
    makeDiscreteParam("C", 2^(-1))
  )
  lrn1 = makeLearner("classif.ksvm", predict.type = "prob")
  lrn2 = makeTuneWrapper(lrn1, resampling = rdesc, measures = list(ber, mmce),
    par.set = ps, control = ctrl, show.info = FALSE)
  
  r = resample(lrn2, iris.task, rdesc, measures = mmce)
  
  expect_identical(sort(names(r$pred$threshold)), c("setosa", "versicolor", "virginica"))
})
