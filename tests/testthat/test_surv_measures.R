context("survival measures")

test_that("survival measures do not do stupid things", {
  requirePackagesOrSkip("glmnet", default.method = "load")

  n = 100
  time = sort(rexp(n, 0.1)) + 1
  status = sample(0:1, n, replace = TRUE, prob = c(1, 10))
  data = data.frame(time = time, status = status,
    x1 = time + rnorm(n, sd = 0.1), x2 = runif(n))
  task = makeSurvTask(id = "dummy", data = data, target = c("time", "status"))

  ms = list(cindex, cindex.uno, iauc.uno)
  learners = listLearners("surv", warn.missing.packages = FALSE)$class
  learners = lapply(learners, makeLearner)

  for (lrn in learners) {
    res = suppressWarnings(resample(lrn, task, resampling = hout, measures = ms,
      models = FALSE, keep.pred = FALSE))
    aggr = res$aggr
    for (measure in ms) {
      r = range(measure$worst, measure$best)
      x = aggr[[sprintf("%s.test.mean", measure$id)]]
      expect_number(x, lower = r[1], upper = r[2],
        label = sprintf("%s/%s", lrn$id, measure$id))
      if (!anyInfinite(r)) {
        expect_true(abs(x - measure$worst) >= abs(x - measure$best),
          label = sprintf("%s/%s", lrn$id, measure$id))
      }
    }
  }
})

test_that("setting measure pars works", {
  mod = train("surv.rpart", wpbc.task)
  pred = predict(mod, wpbc.task)

  measures = list(setMeasurePars(cindex.uno, max.time = 50), cindex.uno)
  perf = performance(pred = pred, task = wpbc.task, model = mod,
    measures = measures)
  expect_true(perf[1] < perf[2])

  measures = list(setMeasurePars(iauc.uno, max.time = 50), iauc.uno)
  perf = performance(pred = pred, task = wpbc.task, model = mod,
    measures = measures)
  expect_true(perf[1] < perf[2])

  measures = list(setMeasurePars(iauc.uno, resolution = 10), iauc.uno)
  perf = performance(pred = pred, task = wpbc.task, model = mod,
    measures = measures)
  expect_string(all.equal(perf[1], perf[2]))
})

test_that("hand constructed tests", {
  requirePackagesOrSkip("Hmisc", default.method = "load")
  n = 100
  time = sort(rexp(n, 0.1)) + 1
  data = data.frame(time = time, status = 1, x1 = order(time))
  task = makeSurvTask(id = "dummy", data = data, target = c("time", "status"))
  mod = suppressWarnings(train("surv.coxph", task))

  pred = predict(mod, task)
  # perfect predictor
  expect_numeric(-getPredictionResponse(pred), sorted = TRUE,
    any.missing = FALSE)

  perf = performance(pred = pred, model = mod, task = task,
    measures = list(cindex, cindex.uno, iauc.uno))
  expect_equal(unname(perf), c(1, 1, 0.99))
})
