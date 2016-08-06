context("regr_featureless")
test_that("regr_featureless", {

  test.measure.learner = function(measure) {
    .task = bh.task
    m.trains.ind = 1:300 # the bh.task has a dataframe of length 506, use 1:300 for traning.
    m.test.ind = 301:506
    y = getTaskTargets(.task)
    n = length(y)
    
    f = function(a) {
      arep = rep(a, n)
      data = data.frame(truth = y, response = arep)
      desc = makeS3Obj("TaskDesc")
      p = makeS3Obj("Prediction", data = data, task.desc = desc)
      measure$fun(pred = p, extra.args = measure$extra.args)
    }
    
    xmin = optimize(f, c(min(y), max(y)), tol = 0.0001, maximum = !measure$minimize)
    p1 = rep(xmin[[1]], length(m.test.ind))
    t_df = getTaskData(.task)
    testSimple(t.name = "regr.featureless", df = t_df[c("crim", "tax", "medv")], target = "medv", 
      train.inds = m.trains.ind, old.predicts = p1, parset = list(measure = measure))
  }
  
  regr.measures = listMeasures2(properties = ("regr"), create = TRUE)
  regr.measures = regr.measures[names(regr.measures) %nin% listMeasures2(properties = c("regr", "req.model"))]
  #it seems that we cannot filter these easily
  regr.measures[["timepredict"]] = NULL
  regr.measures[["arsq"]] = NULL
  lapply(regr.measures, test.measure.learner)
  
  expect_error(train(makeLearner("regr.featureless", measure = auc), regr.num.task), 
    "Measure auc does not support task type regr!")
  
  expect_error(train(makeLearner("regr.featureless", measure = timetrain), regr.num.task), 
    "requires a fitted model")
  
})