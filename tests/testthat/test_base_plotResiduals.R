test_that("plotResiduals with prediction object", {

  set.seed(getOption("mlr.debug.seed"))

  learner = makeLearner("regr.rpart")
  mod = train(learner, regr.task)
  preds = predict(mod, regr.task)
  plot = plotResiduals(preds)
  vdiffr::expect_doppelganger("plotResiduals - regr", plot)

  # histogram
  p_hist = plotResiduals(preds, type = "hist")
  vdiffr::expect_doppelganger("plotResiduals - hist", p_hist)

  # classif
  learner = makeLearner("classif.rpart")
  mod = train(learner, multiclass.task)
  preds = predict(mod, multiclass.task)
  p_hist = plotResiduals(preds)
  vdiffr::expect_doppelganger("plotResiduals - classif", p_hist)

})

test_that("plotResiduals with BenchmarkResult", {
  lrns = list(makeLearner("classif.ksvm"), makeLearner("classif.rpart"))
  tasks = list(multiclass.task, binaryclass.task)
  bmr = benchmark(lrns, tasks, hout, measures = getDefaultMeasure(multiclass.task))

  # scatterplot
  p_scatter = plotResiduals(bmr, type = "scatterplot")
  vdiffr::expect_doppelganger("plotResiduals - scatter - bmr", p_scatter)

  # histogram - bmr
  plotResiduals(bmr, type = "hist")
  p_hist_bmr = plotResiduals(bmr, type = "hist")
  vdiffr::expect_doppelganger("plotResiduals - hist - bmr", p_hist_bmr)

  p_hist_bmr_pretty = plotResiduals(bmr, pretty.names = FALSE)
  vdiffr::expect_doppelganger("plotResiduals - hist - bmr - pretty", p_hist_bmr_pretty)

  # check error when learner short names are not unique
  lrns = list(
    rf = makeLearner("classif.randomForest", id = "rf1"),
    rf2 = makeLearner("classif.randomForest", id = "rf2")
  )
  res = benchmark(lrns, tasks, hout)
  expect_error(plotBMRSummary(res),
    "names are not unique")
})
