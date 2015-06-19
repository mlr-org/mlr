context("classif_svm")

# we cannot do a prob test, as set.seed sems not to work on e1071 svm for the prob parameters!
#requirePackages("e1071", default.method = "load")
#set.seed(1)
#m1=svm(Species~., data=iris, probability=T)
#set.seed(1)
#m2=svm(Species~., data=iris, probability=T)
#all.equal(m1, m2)

test_that("classif_svm", {
  requirePackages("e1071", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  m1 = e1071::svm(multiclass.formula, data=multiclass.train, kernel="radial", gamma=20)
  set.seed(getOption("mlr.debug.seed"))
  m2 = e1071::svm(multiclass.formula, data=multiclass.train, kernel="radial", gamma=20, probability = TRUE)
  p1 = predict(m1, newdata=multiclass.test)
  p2 = predict(m2, newdata=multiclass.test, probability=TRUE)
  testSimple("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, p1,  parset=list(kernel="radial", gamma=20))
  #testProb  ("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, attr(p2, "probabilities"),
  #  parset=list(kernel="radial", gamma=20))

  set.seed(getOption("mlr.debug.seed"))
  m = e1071::svm(multiclass.formula, data=multiclass.train, kernel="sigmoid", gamma=10, probability = TRUE)
  p = predict(m, newdata=multiclass.test, probability = TRUE)
  #testProb  ("classif.svm",multiclass.df, multiclass.target, multiclass.train.inds, attr(p2, "probabilities"),
  #  parset=list(kernel="sigmoid", gamma=10))

  set.seed(getOption("mlr.debug.seed"))
  m = e1071::svm(multiclass.formula, data=multiclass.train, kernel="polynomial", degree=3, coef0=2, gamma=1.5)
  p = predict(m, newdata=multiclass.test)
  p2 = predict(m, newdata=multiclass.test)
  testSimple("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, p,
    parset=list(kernel="polynomial", degree=3, coef0=2, gamma=1.5))
  #testProb  ("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, attr(p2, "probabilities"),
  #  parset=list(kernel="polynomial", degree=3, coef0=2, gamma=1.5))

  tt = function (formula, data, subset=1:150, ...) {
    e1071::svm(formula, data=data[subset,], kernel="polynomial", degree=3, coef0=2, gamma=1.5)
  }

  testCV("classif.svm", multiclass.df, multiclass.target, tune.train=tt, parset=list(kernel="polynomial", degree=3, coef0=2, gamma=1.5))

  lrn = makeLearner("classif.svm", scale = FALSE)
  model = train(lrn, multiclass.task)
  preds = predict(model, multiclass.task)
  expect_equal(length(preds), 5)
  lrn = makeLearner("classif.svm", scale = c(TRUE))
  model = train(lrn, multiclass.task)
  preds = predict(model, multiclass.task)
  expect_equal(length(preds), 5)
})
