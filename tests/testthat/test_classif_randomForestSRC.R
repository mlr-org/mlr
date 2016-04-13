context("classif_randomForestSRC")

test_that("classif_randomForestSRC", {
  requirePackagesOrSkip("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 100),
    list(ntree = 250, mtry.ratio = 0.9),
    list(ntree = 250, nodesize = 2, na.action = "na.impute", importance = "permute", proximity = FALSE)
  )
  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    if (!is.null(parset$mtry.ratio)) {
      parset$mtry = parset$mtry.ratio * (ncol(binaryclass.df) - 1)
      parset$mtry.ratio = NULL
    } 
    parset = c(parset, list(data = binaryclass.train, formula = binaryclass.formula, forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrc, parset)
    p = predict(m, newdata = binaryclass.test, membership = FALSE, na.action = "na.impute")
    old.predicts.list[[i]] = p$class
    old.probs.list[[i]] = p$predicted[,1]
  }

  testSimpleParsets("classif.randomForestSRC", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets ("classif.randomForestSRC", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})
