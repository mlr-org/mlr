# test that a given learner respects its weights tag. we do this:
# train without weights, with weights = 1, and with changed weights
# then we check that changed weights actually change the output
# note: it is not always easy to find a good data and weights combo to reliably achieve this,
# as for some learners we need very extreme weights and for others exteme weights cause error.
#
# args: we pass a learner, task, train and test.inds, and weight vec for train inds
# hyperpars is the list we set up in learners all where we need to deviate from the defaults for stability
# we can also set pred.type and the getter for the output col from the preds.

testThatLearnerRespectsWeights = function(lrn, task, train.inds, test.inds, weights, hyperpars,
  pred.type, get.pred.fun) {

  lrn = setPredictType(lrn, pred.type)

  if (lrn$id %in% names(hyperpars))
    lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])

  rin = makeResampleInstance("Holdout", task = task)
  m1 = train(lrn, task, subset = train.inds)
  w.allone = rep(1, length(train.inds))
  m2 = train(lrn, task, subset = train.inds, weights = w.allone)
  m3 = train(lrn, task, subset = train.inds, weights = weights)
  p1 = predict(m1, task, subset = test.inds)
  p2 = predict(m2, task, subset = test.inds)
  p3 = predict(m3, task, subset = test.inds)
  perf1 = performance(p1)
  perf2 = performance(p2)
  perf3 = performance(p3)
  expect_true(!is.na(perf1), info = lrn$id)
  expect_true(!is.na(perf2), info = lrn$id)
  expect_true(!is.na(perf3), info = lrn$id)
  expect_equal(get.pred.fun(p1), get.pred.fun(p2), info = lrn$id)
  expect_true(any(get.pred.fun(p1) != get.pred.fun(p3)), info = lrn$id)
}

