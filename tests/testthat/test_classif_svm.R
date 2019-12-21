context("classif_svm")

# we cannot do a prob test, as set.seed sems not to work on e1071 svm for the prob parameters!
# requirePackagesOrSkip("e1071", default.method = "load")
# set.seed(1)
# m1=svm(Species~., data=iris, probability=T)
# set.seed(1)
# m2=svm(Species~., data=iris, probability=T)
# all.equal(m1, m2)
# UPD 12/2019: The above issue seems to be solved. However, we have no param
# "probability" in "classif.svm"?

test_that("classif_svm", {
  requirePackagesOrSkip("e1071", default.method = "load")

  parset.list = list(
    list(),
    list(gamma = 20),
    list(kernel = "sigmoid", gamma = 10),
    list(kernel = "polynomial", degree = 3, coef0 = 2, gamma = 1.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    m1 = do.call(e1071::svm, pars)
    pars$probability = TRUE
    m2 = do.call(e1071::svm, pars)
    old.predicts.list[[i]] = predict(m1, newdata = multiclass.test)
    old.probs.list[[i]] = predict(m2, newdata = multiclass.test,
      probability = TRUE)
  }

  testSimpleParsets("classif.svm", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  # testProbParsets("classif.svm", multiclass.df, multiclass.target,
  #  multiclass.train.inds, old.probs.list, parset.list2)

  tt = function(formula, data, subset = 1:150, ...) {
    e1071::svm(formula, data = data[subset, ], kernel = "polynomial",
      degree = 3, coef0 = 2, gamma = 1.5)
  }

  testCV("classif.svm", multiclass.df, multiclass.target, tune.train = tt,
    parset = list(kernel = "polynomial", degree = 3, coef0 = 2, gamma = 1.5))

  lrn = makeLearner("classif.svm", scale = FALSE)
  model = train(lrn, multiclass.task)
  preds = predict(model, multiclass.task)
  expect_lt(performance(preds), 0.3)

  lrn = makeLearner("classif.svm", scale = TRUE)
  model = train(lrn, multiclass.task)
  preds = predict(model, multiclass.task)
  expect_lt(performance(preds), 0.3)
})

test_that("classif_svm with many features", {
  xt = cbind(as.data.frame(matrix(rnorm(4e4), ncol = 2e4)),
    x = as.factor(c("a", "b")))
  xt.task = makeClassifTask("xt", xt, "x")
  # the given task has many features, the formula interface fails
  expect_silent(train("classif.svm", xt.task))
})
