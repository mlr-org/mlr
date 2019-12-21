context("classif_boosting")

# FIXME: Test takes 80s if we test with the default mfinal=100. Without 10s
# Is there any reasons why testing mfinal=100 would be absolutely necessary?

test_that("classif_boosting rpart control", {
  requirePackagesOrSkip(c("adabag", "rpart"), default.method = "load")

  parset.list1 = list(
    # list(control = rpart::rpart.control(xval = 0)), # mfinal=100
    list(mfinal = 1, control = rpart::rpart.control(xval = 0)),
    list(mfinal = 2, control = rpart::rpart.control(cp = 0.2, xval = 0))
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
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

  parset.list2 = list(
    #list(), # mfinal=100
    list(mfinal = 1),
    list(mfinal = 2, cp = 0.2)
  )

  testSimpleParsets("classif.boosting", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.boosting", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list2)
})

test_that("classif_boosting cv", {
  # cv testing with an empty parameter list, takes too long (default mfinal = 100L)
  parset.list3 = list(
    list(mfinal = 1),
    list(mfinal = 2, cp = 0.2)
  )

  testCVParsets("classif.boosting", multiclass.df, multiclass.target,
    tune.train = boosting_helper1, tune.predict = boosting_helper2, parset.list = parset.list3)
})
