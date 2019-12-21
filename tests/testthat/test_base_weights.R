context("weights")

test_that("weights", {
  lrn = makeLearner("regr.lm")
  ws = seq_len(nrow(regr.df))
  rt = makeRegrTask(target = regr.target, data = regr.df)
  m = train(lrn, task = rt, weights = ws)
  p = predict(m, task = rt, subset = 30:100)
  df = as.data.frame(p)
  cns = colnames(df)
  expect_equal(cns, c("id", "truth", "response"))

  # with weights in task
  rt.w = makeRegrTask(target = regr.target, data = regr.df, weights = ws)
  m.w = train(lrn, task = rt.w)
  p.w = predict(m.w, task = rt.w, subset = 30:100)
  df.w = as.data.frame(p.w)
  expect_equal(df.w$response, df$response)
  inds = seq(1, 10, 2)
  rt.w2 = subsetTask(rt.w, inds, features = getTaskFeatureNames(rt.w))
  expect_equal(rt.w2$weights, ws[inds])

  # glm bug, we need do.call
  m2 = do.call(lm, list(regr.formula, data = regr.df, weights = ws))
  p2 = predict(m2, newdata = regr.df[30:100, ])
  expect_equal(p2, p$data$response, check.attributes = FALSE)

  expect_error(train(lrn, rger.task, weights = 1:2))
})

test_that("weights remain after subset", {
  tasks = list(binaryclass.task, multiclass.task, multilabel.task, regr.task, surv.task, noclass.task)
  for (t in tasks) {
    expect_false(getTaskDesc(t)$has.weights)
    ws = seq_len(getTaskDesc(t)$size)
    wtask = changeData(t, weights = ws)
    expect_true(getTaskDesc(wtask)$has.weights)
    expect_equal(wtask$weights, ws)
    expect_equal(subsetTask(wtask, 1:10, features = getTaskFeatureNames(wtask))$weights, 1:10)
    expect_true(getTaskDesc(subsetTask(wtask, 1:10, getTaskFeatureNames(wtask)))$has.weights)
  }
})
