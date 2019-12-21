context("tuneThreshold")

test_that("tuneThreshold", {

  # binary classes, 1 th
  lrn = makeLearner("classif.lda", predict.type = "prob")
  m = train(lrn, binaryclass.task)
  p = predict(m, binaryclass.task)
  tr = tuneThreshold(p)
  expect_true(length(tr$th) == 1 && tr$th >= 0 && tr$th <= 1)
  expect_true(tr$perf >= 0 && tr$perf < 0.3)

  # multiclass
  m = train(lrn, multiclass.task)
  p = predict(m, multiclass.task)
  tr = tuneThreshold(p, mmce, control = list(maxit = 5L))
  expect_true(length(tr$th) == 3 && all(tr$th >= 0) && all(tr$th <= 1))
  expect_true(tr$perf >= 0 && tr$perf < 0.1)
})

test_that("tuneThreshold works with all tuning methods", {
  lrn = makeLearner("classif.lda", predict.type = "prob")
  ps = makeParamSet(makeNumericParam("nu", lower = 2, upper = 3))
  ctrls = list(
    gensa = makeTuneControlGenSA(start = list(nu = 2.5), maxit = 1,
      tune.threshold = TRUE),
    cmaes = makeTuneControlCMAES(start = list(nu = 2.5), maxit = 1,
      tune.threshold = TRUE),
    design = makeTuneControlDesign(design = generateDesign(n = 2, par.set = ps),
      tune.threshold = TRUE),
    grid = makeTuneControlGrid(resolution = 2L, tune.threshold = TRUE),
    irace = makeTuneControlIrace(maxExperiments = 12, nbIterations = 1L,
      minNbSurvival = 1, tune.threshold = TRUE)
  )
  for (ctrl in ctrls) {
    lrn.tuned = makeTuneWrapper(lrn, resampling = cv2, measures = acc,
      par.set = ps, control = ctrl)
    res = resample(lrn.tuned, binaryclass.task,
      resampling = makeResampleDesc("Holdout"), extract = getTuneResult)
    expect_number(res$extract[[1]]$threshold)
  }
})
