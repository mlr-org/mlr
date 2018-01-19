context("classif_randomForestSRCSyn")

test_that("classif_randomForestSRCSyn", {
  requirePackagesOrSkip("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 10L),
    list(ntree = 5L, mtry = 4L),
    list(ntree = 5L, nodesize = 2L, nsplit = 5, splitrule = "random")
  )
  old.predicts.list = list()
  old.probs.list = list()

  ## binary
  for (i in 1L:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = binaryclass.train, formula = binaryclass.formula, forest = TRUE, na.action = "na.impute",
      verbose = FALSE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrcSyn, parset)
    # seed needed because with few trees sometimes probabilities 0.5 occur
    set.seed(getOption("mlr.debug.seed"))
    p = randomForestSRC::rfsrcSyn(object = m, newdata = binaryclass.test, na.action = "na.impute", verbose = FALSE, membership = FALSE)$rfSynPred
    old.predicts.list[[i]] = p$class
    old.probs.list[[i]] = p$predicted[,binaryclass.class.levs[1]]
  }

  testSimpleParsets("classif.randomForestSRCSyn", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets ("classif.randomForestSRCSyn", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)

  ## multiclass
  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1L:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = multiclass.train, formula = multiclass.formula, forest = TRUE, na.action = "na.impute",
      verbose = FALSE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrcSyn, parset)
    p = randomForestSRC::rfsrcSyn(object = m, newdata = multiclass.test, na.action = "na.impute", verbose = FALSE, membership = FALSE)$rfSynPred
    old.predicts.list[[i]] = p$class
    old.probs.list[[i]] = p$predicted
  }

  testSimpleParsets("classif.randomForestSRCSyn", multiclass.df, multiclass.target, multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets ("classif.randomForestSRCSyn", multiclass.df, multiclass.target, multiclass.train.inds, old.probs.list, parset.list)

})
