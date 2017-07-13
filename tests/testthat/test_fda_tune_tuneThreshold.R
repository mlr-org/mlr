context("fdaTuneThreshold")

test_that("tuneThreshold works with fda learner", {
  # fdaclassif
  lrn = makeLearner("classif.fdaknn", predict.type = "prob")
  m = train(lrn, fda.binary.gp.task.small)
  p = predict(m, fda.binary.gp.task.small)
  tr = tuneThreshold(p)
  expect_true(length(tr$th) == 1 && tr$th >= 0 && tr$th <= 1)
  expect_true(tr$perf >= 0 && tr$perf < 0.1)
})


test_that("tuneThreshold works with FDA classif task", {
  lrn = makeLearner("classif.fdaknn", predict.type = "prob")
  ps = makeParamSet(makeIntegerParam("knn", lower = 1L, upper = 5L))
  ctrls = list(
    # FIXME: GenSA throws error when using integer
    # gensa = makeTuneControlGenSA(start = list(knn = 2L), maxit = 1, tune.threshold = TRUE),
    cmaes = makeTuneControlCMAES(start = list(knn = 2L), maxit = 1, tune.threshold = TRUE),
    design = makeTuneControlDesign(design = generateDesign(n = 2, par.set = ps),  tune.threshold = TRUE),
    grid = makeTuneControlGrid(resolution = 2L, tune.threshold = TRUE),
    irace = makeTuneControlIrace(maxExperiments = 12, nbIterations = 1L, minNbSurvival = 1, tune.threshold = TRUE)
  )
  for (ctrl in ctrls) {
    lrn.tuned = makeTuneWrapper(lrn, resampling = cv2, measures = acc, par.set = ps, control = ctrl)
    res = resample(lrn.tuned, fda.binary.gp.task.small, resampling = makeResampleDesc("Holdout"), extract = getTuneResult)
    expect_number(res$extract[[1]]$threshold)
  }

})
