context("classif_blackboost")

test_that("classif_blackboost", {
  requirePackagesOrSkip(c("mboost", "party"), default.method = "load")

  parset.list1 = list(
    list(family = mboost::Binomial()),
    list(family = mboost::Binomial(), 
      control = mboost::boost_control(mstop = 10L),
      tree_controls = party::ctree_control(teststat = "max", testtype = "Teststatistic",
        mincriterion = 0, maxdepth = 4, savesplitstats = FALSE)),
    list(family = mboost::Binomial(link = "probit"), control = mboost::boost_control(mstop = 10L, nu = 0.03))
  )

  parset.list2 = list(
    list(),
    list(mstop = 10L, maxdepth = 4),
    list(Binomial.link = "probit", mstop = 10L, nu = 0.03)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::blackboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata = binaryclass.test, type = "class")
    set.seed(getOption("mlr.debug.seed"))
    old.probs.list[[i]] = predict(m, newdata = binaryclass.test, type = "response")[,1]
  }

  testSimpleParsets("classif.blackboost", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.blackboost", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list2)
})


test_that("classif_blackboost works with family PropOdds", {
  new.binary.df = binaryclass.df
  new.binary.df[,binaryclass.target] = as.ordered(new.binary.df[,binaryclass.target])
  new.classif.train = new.binary.df[binaryclass.train.inds,]
  new.classif.test = new.binary.df[binaryclass.test.inds,]
  parset.list1 = list(family = mboost::PropOdds())
  parset.list2 = list(family = "PropOdds")
  old.predicts.list = list()
  old.probs.list = list()
  parset = parset.list1
  pars = list(binaryclass.formula, data = new.classif.train)
  pars = c(pars, parset)
  set.seed(getOption("mlr.debug.seed"))
  m = do.call(mboost::blackboost, pars)
  set.seed(getOption("mlr.debug.seed"))
  old.predicts.list = predict(m, newdata = new.classif.test, type = "class")
  set.seed(getOption("mlr.debug.seed"))
  old.probs.list = predict(m, newdata = new.classif.test, type = "response")[,1]
  
  testSimple("classif.blackboost", new.binary.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list2)
  testProb("classif.blackboost", new.binary.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list2)

})


test_that("classif_blackboost probability predictions with family 'AUC' and 'AdaExp'", {
  families = list("AUC", "AdaExp")
  lapply(families, FUN = function(x){
    lrn = makeLearner("classif.blackboost", par.vals = list(family = x), predict.type = "prob")
    mod = train(lrn, binaryclass.task)
    expect_error(predict(mod, binaryclass.task), "Predictions of probabilities")
  })
}) 
   



