context("classif_featureless")

test_that("classif_featureless", {
  # computes priori probabilities
  getPriorProbs = function(task) {
    response = getTaskTargets(task)
    prop.table(table(response))
  }
  ps.list = list(
    list(method = "majority", ties.method = "first"),
    list(method = "majority", ties.method = "last"),
    list(method = "majority", ties.method = "random"),
    list(method = "sample-prior")
  )

  # MULTICLASS, check two cases:
  # 1) sample training instances so that not all classes have equal size
  # 2) use multiclass.train.inds (3 equally sized classes)

  n = length(multiclass.train.inds)
  set.seed(getOption("mlr.debug.seed"))
  inds = list(
    sample(seq_row(multiclass.df), n),
    multiclass.train.inds
  )

  for(train.inds in inds) {
    test.inds = setdiff(seq_row(multiclass.df), train.inds)
    # compute prior probabilities
    prior = getPriorProbs(subsetTask(multiclass.task, train.inds))
    lvls = names(prior)
    p = matrix(prior, nrow = length(test.inds), ncol = length(prior), byrow = TRUE)
    colnames(p) = lvls

    # predict majority class
    p1.first = factor(getMaxIndexOfRows(p, ties.method = "first"),
      levels = seq_along(lvls), labels = lvls)
    p1.last = factor(getMaxIndexOfRows(p, ties.method = "last"),
      levels = seq_along(lvls), labels = lvls)
    set.seed(getOption("mlr.debug.seed"))
    p1.random = factor(getMaxIndexOfRows(p, ties.method = "random"),
      levels = seq_along(lvls), labels = lvls)
    # sample class according to prior probabilities
    set.seed(getOption("mlr.debug.seed"))
    p2 = factor(sample(lvls, size = nrow(p), prob = prior, replace = TRUE), levels = lvls)

    testSimpleParsets(t.name = "classif.featureless", df = multiclass.df,
      target = multiclass.target, train.inds = train.inds,
      old.predicts.list = list(p1.first, p1.last, p1.random, p2), parset = ps.list)
    testProbParsets(t.name = "classif.featureless", df = multiclass.df,
      target = multiclass.target, train.inds = train.inds,
      old.probs.list = list(p, p, p, p), parset = ps.list)
  }

  # BINARYCLASS:
  # compute prior probabilities
  positive = getTaskDescription(binaryclass.task)$positive
  prior = getPriorProbs(subsetTask(binaryclass.task, subset = binaryclass.train.inds))
  p = matrix(prior, nrow = length(binaryclass.test.inds), ncol = length(prior), byrow = TRUE)
  colnames(p) = binaryclass.class.levs

  # predict majority class
  p1.first = factor(getMaxIndexOfRows(p, ties.method = "first"),
    levels = seq_along(binaryclass.class.levs),
    labels = binaryclass.class.levs)
  p1.last = factor(getMaxIndexOfRows(p, ties.method = "last"),
    levels = seq_along(binaryclass.class.levs),
    labels = binaryclass.class.levs)
  set.seed(getOption("mlr.debug.seed"))
  p1.random = factor(getMaxIndexOfRows(p, ties.method = "random"),
    levels = seq_along(binaryclass.class.levs),
    labels = binaryclass.class.levs)

  # sample class according to prior probabilities
  set.seed(getOption("mlr.debug.seed"))
  p2 = sample(binaryclass.class.levs, size = length(binaryclass.test.inds),
    prob = prior, replace = TRUE)
  p2 = factor(p2, levels = binaryclass.class.levs)

  testSimpleParsets(t.name = "classif.featureless", df = binaryclass.df,
    target = binaryclass.target, train.inds = binaryclass.train.inds,
    old.predicts.list = list(p1.first, p1.last, p1.random, p2), parset = ps.list)
  testProbParsets(t.name = "classif.featureless", df = binaryclass.df,
    target = binaryclass.target, train.inds = binaryclass.train.inds,
    old.probs.list = list(p[,positive], p[,positive], p[,positive], p[,positive]), parset = ps.list)

  # test that printers work correctly
  lrn = makeLearner("classif.featureless")
  expect_output(print(lrn), "featureless")
})
