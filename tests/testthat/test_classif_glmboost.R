context("classif_glmboost")

test_that("classif_glmboost", {
  requirePackagesOrSkip("mboost", default.method = "load")

  parset.list1 = list(
    list(family = mboost::Binomial()),
    list(family = mboost::Binomial(), control = mboost::boost_control(nu = 0.03)),
    list(family = mboost::Binomial(link = "probit"), control = mboost::boost_control(mstop = 600), center = TRUE)
  )

  parset.list2 = list(
    list(),
    list(family = "Binomial", nu=0.03),
    list(family = "Binomial", Binomial.link = "probit", mstop = 600, center = TRUE)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(binaryclass.formula, data=binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::glmboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata=binaryclass.test, type="class")
    set.seed(getOption("mlr.debug.seed"))
    old.probs.list[[i]] = 1 - predict(m, newdata=binaryclass.test, type="response")[,1]
  }

  testSimpleParsets("classif.glmboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.glmboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list2)
})

test_that("classif_glmboost works with family PropOdds", {
  new.binary.df = binaryclass.df
  new.binary.df[,binaryclass.target] = as.ordered(new.binary.df[,binaryclass.target])
  new.classif.train = new.binary.df[binaryclass.train.inds,]
  new.classif.test = new.binary.df[binaryclass.test.inds,]
  parset.list1 = list(family = mboost::PropOdds())
  parset.list2 = list(family = "PropOdds")
  old.predicts.list = list()
  old.probs.list = list()
  parset = parset.list1#[[i]]
  pars = list(binaryclass.formula, data = new.classif.train)
  pars = c(pars, parset)
  set.seed(getOption("mlr.debug.seed"))
  m = do.call(mboost::glmboost, pars)
  set.seed(getOption("mlr.debug.seed"))
  old.predicts.list = predict(m, newdata = new.classif.test, type = "class")
  set.seed(getOption("mlr.debug.seed"))
  old.probs.list = 1 - predict(m, newdata = new.classif.test, type = "response")[,1]
  
  testSimple("classif.glmboost", new.binary.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list2)
  testProb("classif.glmboost", new.binary.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list2)
  
})

