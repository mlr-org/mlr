context("classif_randomForestSRCSyn")

test_that("classif_randomForestSRCSyn", {
  requirePackages("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 10L),
    list(ntree = 25L, mtry = 4L),
    list(ntree = 25L, nodesize = 2L, na.action = "na.impute")
  )
  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1L:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = binaryclass.train, formula = binaryclass.formula, newdata = binaryclass.test,
     importance = "none", proximity = FALSE, forest = TRUE, verbose = FALSE))
    set.seed(getOption("mlr.debug.seed"))
    mod = do.call(randomForestSRC::rfsrcSyn, parset)
    max.id = apply(mod$rfSynPred$predicted, MARGIN = 1L, which.max)
    p = factor(colnames(mod$rfSynPred$predicted)[max.id])
    old.predicts.list[[i]] = p
    probs = mod$rfSynPred$predicted[, binaryclass.task$task.desc$positive]
    old.probs.list[[i]] = probs
  }

  testSimpleParsets("classif.randomForestSRCSyn", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets ("classif.randomForestSRCSyn", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})
