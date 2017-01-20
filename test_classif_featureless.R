load_all()

configureMlr(show.learner.output = TRUE, on.learner.error = "stop")
options(warn = 2)

# lrn = makeLearner("classif.dummy", method = "sample-prior", predict.type = "prob")
# m = train(lrn, iris.task)
# p = predict(m, iris.task)
# print(summary(as.data.frame(p)$response))
# print(head(as.data.frame(p)))
#
lrn = makeLearner("regr.dummy", method = "median", predict.type = "se")
m = train(lrn, bh.task)
p = predict(m, bh.task)
print(summary(as.data.frame(p)$response))
print(head(as.data.frame(p)))
#
context("classif_featureless")

test_that("classif_featureless", {
  # MULTICLASS:
  # sample training instances so that not all classes have equal size
  set.seed(getOption("mlr.debug.seed"))
  n = 100
  train.inds = sample(seq_row(multiclass.df), n)
  test.inds = setdiff(seq_row(multiclass.df), train.inds)
  lvls = getTaskClassLevels(multiclass.task)

  # compute prior probabilities
  prior = prop.table(table(getTaskTargets(subsetTask(multiclass.task, subset = train.inds))))
  p = setColNames(matrix(prior, nrow = length(test.inds), ncol = length(prior), byrow = TRUE), lvls)

  # predict majority class
  p1 = factor(getMaxIndexOfRows(p), levels = seq_along(lvls), labels = lvls)
  # sample class according to prior probabilities
  set.seed(getOption("mlr.debug.seed"))
  p2 = factor(sample(lvls, size = length(test.inds), prob = prior, replace = TRUE))

  p.class = list(p1, p2)
  p.prob = list(p, p)
  ps.list = list(list(method = "majority"), list(method = "sample-prior"))

  testSimpleParsets(t.name = "classif.featureless", df = multiclass.df,
    target = multiclass.target, train.inds = train.inds,
    old.predicts.list = p.class, parset = ps.list)
  testProbParsets(t.name = "classif.featureless", df = multiclass.df,
    target = multiclass.target, train.inds = train.inds,
    old.probs.list = p.prob, parset = ps.list)

  # BINARYCLASS: TODO
  # compute prior probabilities
  prior = prop.table(table(getTaskTargets(subsetTask(binaryclass.task, subset = binaryclass.train.inds))))
  p = setColNames(matrix(prior, nrow = length(binaryclass.test.inds), ncol = length(prior), byrow = TRUE), binaryclass.class.levs)

  # predict majority class
  p1 = factor(getMaxIndexOfRows(p), levels = seq_along(binaryclass.class.levs), labels = binaryclass.class.levs)
  # sample class according to prior probabilities
  set.seed(getOption("mlr.debug.seed"))
  p2 = factor(sample(binaryclass.class.levs, size = length(binaryclass.test.inds), prob = prior, replace = TRUE))

  p.class = list(p1, p2)
  p.prob = list(p, p)
  ps.list = list(list(method = "majority"), list(method = "sample-prior"))

  testSimpleParsets(t.name = "classif.featureless", df = multiclass.df,
    target = multiclass.target, train.inds = train.inds,
    old.predicts.list = p.class, parset = ps.list)
  testProbParsets(t.name = "classif.featureless", df = multiclass.df,
    target = multiclass.target, train.inds = train.inds,
    old.probs.list = p.prob, parset = ps.list)




  testSimple("classif.featureless", binaryclass.df, binaryclass.target, binaryclass.inds, p.class)
  testProb("classif.featureless", binaryclass.df, binaryclass.target, binaryclass.inds, p.prob)

  tt = function(formula, data) {glm(formula, data = data, family = binomial)}
  tp = function(model, newdata) {
    p = predict(model, newdata, type = "response")
    as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
  }

  testCV("classif.featureless", binaryclass.df, binaryclass.target, tune.train = tt, tune.predict = tp )

  # test that bad measures cannot be used
  expect_error(train(makeLearner("classif.featureless", measure = auc), iris.task),
    "Multiclass problems cannot be used for measure auc!")
  expect_error(train(makeLearner("classif.featureless", measure = timetrain), binaryclass.task),
    "requires a fitted model")
  expect_error(train(makeLearner("classif.featureless", measure = mse), binaryclass.task),
    "Measure mse does not support task type classif!")

  # test that printers work correctly and print the measure id and not <measure>
  lrn = makeLearner("classif.featureless")
  expect_output(print(lrn), "majority")
  expect_output(print(getHyperPars(lrn)), "majority")

  setMlrOption("debug.seed", seed)
})
