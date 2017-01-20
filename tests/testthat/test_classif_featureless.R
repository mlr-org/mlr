context("classif_featureless")

test_that("classif_featureless", {
  # computes priori probabilities
  getPriorProbs = function(task) {
    response = getTaskTargets(task)
    prop.table(table(response))
  }
  ps.list = list(
    list(method = "majority"),
    list(method = "sample-prior")
  )

  # MULTICLASS:
  # sample training instances so that not all classes have equal size
  n = 100
  set.seed(getOption("mlr.debug.seed"))
  train.inds = sample(seq_row(multiclass.df), n)
  test.inds = setdiff(seq_row(multiclass.df), train.inds)

  # compute prior probabilities
  prior = getPriorProbs(subsetTask(multiclass.task, train.inds))
  lvls = names(prior)
  p = matrix(prior, nrow = length(test.inds), ncol = length(prior), byrow = TRUE)
  colnames(p) = lvls

  # predict majority class
  p1 = factor(getMaxIndexOfRows(p), levels = seq_along(lvls), labels = lvls)
  # sample class according to prior probabilities
  set.seed(getOption("mlr.debug.seed"))
  p2 = factor(sample(lvls, size = nrow(p), prob = prior, replace = TRUE), levels = lvls)

  testSimpleParsets(t.name = "classif.featureless", df = multiclass.df,
    target = multiclass.target, train.inds = train.inds,
    old.predicts.list = list(p1, p2), parset = ps.list)
  testProbParsets(t.name = "classif.featureless", df = multiclass.df,
    target = multiclass.target, train.inds = train.inds,
    old.probs.list = list(p, p), parset = ps.list)

  # BINARYCLASS:
  # compute prior probabilities
  positive = getTaskDescription(binaryclass.task)$positive
  prior = getPriorProbs(subsetTask(binaryclass.task, subset = binaryclass.train.inds))
  p = matrix(prior, nrow = length(binaryclass.test.inds), ncol = length(prior), byrow = TRUE)
  colnames(p) = binaryclass.class.levs

  # predict majority class
  p1 = factor(getMaxIndexOfRows(p), levels = seq_along(binaryclass.class.levs),
    labels = binaryclass.class.levs)
  # sample class according to prior probabilities
  set.seed(getOption("mlr.debug.seed"))
  p2 = sample(binaryclass.class.levs, size = length(binaryclass.test.inds),
    prob = prior, replace = TRUE)
  p2 = factor(p2, levels = binaryclass.class.levs)

  testSimpleParsets(t.name = "classif.featureless", df = binaryclass.df,
    target = binaryclass.target, train.inds = binaryclass.train.inds,
    old.predicts.list = list(p1, p2), parset = ps.list)
  testProbParsets(t.name = "classif.featureless", df = binaryclass.df,
    target = binaryclass.target, train.inds = binaryclass.train.inds,
    old.probs.list = list(p[,positive], p[,positive]), parset = ps.list)

  # test that printers work correctly
  lrn = makeLearner("classif.featureless")
  expect_output(print(lrn), "featureless")
})
