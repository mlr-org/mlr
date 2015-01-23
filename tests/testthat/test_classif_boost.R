context("classif_boosting")

test_that("classif_boosting", {
  requirePackages(c("adabag", "rpart"), default.method = "load")
  parset.list1 = list(
    list(mfinal = 1, control = rpart::rpart.control(xval = 0)),
    list(mfinal = 2, control = rpart::rpart.control(cp = 0.2, xval = 0))
  )
  parset.list2 = list(
    list(mfinal = 1),
    list(mfinal = 2, cp = 0.2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(adabag::boosting, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = multiclass.test)
    old.predicts.list[[i]] = as.factor(p$class)
    old.probs.list[[i]] = setColNames(p$prob, levels(multiclass.df[, multiclass.target]))
  }

  testSimpleParsets("classif.boosting", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.boosting", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list2)

  tt = function (formula, data, subset = 1:nrow(data), ...) {
    args = list(...)
    if (!is.null(args$cp))
      ctrl = rpart::rpart.control(cp = args$cp, xval = 0)
    else
      ctrl = rpart::rpart.control(xval = 0)
    set.seed(getOption("mlr.debug.seed"))
    adabag::boosting(formula, data[subset,], mfinal = args$mfinal, control = ctrl)
  }

  tp = function(model, newdata) {
    set.seed(getOption("mlr.debug.seed"))
    as.factor(predict(model, newdata)$class)
  }

  testCVParsets("classif.boosting", multiclass.df, multiclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list2)
})

