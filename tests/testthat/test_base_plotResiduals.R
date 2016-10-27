context("plotResiduals")

test_that("plotResiduals with prediction object", {
  learner = makeLearner("classif.rpart")
  mod = train(learner, multiclass.task)
  preds = predict(mod, multiclass.task)
  plotResiduals(preds)
  ggsave(tempfile(fileext = ".png"))
  plotResiduals(preds, type = "hist")
  ggsave(tempfile(fileext = ".png"))
})

test_that("plotResiduals with BenchmarkResult", {
  lrns = list(makeLearner("classif.nnet"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  bmr = benchmark(lrns, tasks, hout)
  plotResiduals(bmr)
  ggsave(tempfile(fileext = ".png"))
  plotResiduals(bmr, type = "hist")
  ggsave(tempfile(fileext = ".png"))
})
