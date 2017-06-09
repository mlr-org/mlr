context("survival measures")

test_that("survival measures do not do stupid things", {
  set.seed(1)
  n = 100
  time = sort(rexp(n, 0.1)) + 1
  status = sample(0:1, n, replace = TRUE, prob = c(1, 10))
  data = data.frame(time = time, status = status, x1 = time + rnorm(n, sd = 0.1), x2 = runif(n))
  task = makeSurvTask(id = "dummy", data = data, target = c("time", "status"))

  ms = list(cindex, cindex.uno, iauc.uno)
  learners = c("surv.coxph", "surv.CoxBoost", "surv.rpart", "surv.ranger", "surv.cvglmnet", "surv.glmnet", "surv.gamboost", "surv.glmboost", "surv.randomForestSRC", "surv.cforest")
  learners = lapply(learners, makeLearner)

  for (lrn in learners) {
    res = suppressWarnings(resample(lrn, task, resampling = hout, measures = ms, models = FALSE, keep.pred = FALSE))
    aggr = res$aggr
    for (measure in ms) {
      r = range(measure$worst, measure$best)
      x = aggr[[sprintf("%s.test.mean", measure$id)]]
      expect_number(x, lower = r[1], upper = r[2], label = sprintf("%s/%s", lrn$id, measure$id))
      if (!anyInfinte(r))
        expect_true(abs(x - measure$worst) >= abs(x - measure$best), label = sprintf("%s/%s", lrn$id, measure$id))
    }
  }
})
