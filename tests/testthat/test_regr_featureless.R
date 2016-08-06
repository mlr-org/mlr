context("regr_featureless")
test_that("regr_featureless", {
  m.trains.ind = 1:300 # the bh.task has a dataframe of lenght 506, use 1:300 for traning.
  m.test.ind = 301:506
  getBestPredictionR = function(.task,measure)
  {
    y = getTaskTargets(.task)
    n = length(y)
    f = function (a) {
      arep = rep(a, n)
      data = data.frame(truth = y, response = arep)
      desc = makeS3Obj("TaskDesc")
      p = makeS3Obj("Prediction", data = data, task.desc = desc)
      measure$fun(pred = p, extra.args = measure$extra.args)
    }
    xmin <- optimize(f, c(min(y), max(y)), tol = 0.0001)
  }
  rst = getBestPredictionR(bh.task,mae)
  p1 = rep(rst$minimum,length(m.test.ind))
  t_df = getTaskData(bh.task)
  testSimple(
    t.name = "regr.featureless",
    df = t_df[c("crim", "tax", "medv")],
    target = "medv",
    train.inds = m.trains.ind,
    old.predicts = p1,
    parset = list(measure = mae)
  )
})