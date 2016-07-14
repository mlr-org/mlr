context("multilabel_randomForestSRC")

test_that("multilabel_randomForestSRC", {
  requirePackagesOrSkip("randomForestSRC", default.method = "load")
  
  parset.list = list(
    list(),
    list(ntree = 100),
    list(ntree = 250, mtry = 5L),
    list(ntree = 250, nodesize = 2, na.action = "na.impute", importance = "permute", proximity = FALSE)
  )
  old.predicts.list = list()
  old.probs.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    for(i in multilabel.target) {
      multilabel.train[i] = factor(multilabel.train[[i]], levels = c("TRUE", "FALSE"))
      multilabel.test[i] = factor(multilabel.test[[i]], levels = c("TRUE", "FALSE"))
    }
    parset = c(parset, list(data = multilabel.train, formula = multilabel.formula.cbind, forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrc, parset)
    p = predict(m, newdata = multilabel.test, membership = FALSE, na.action = "na.impute")
    old.predicts.list[[i]] = data.frame(sapply(p$classOutput, function(x) as.logical(x$class)))
    old.probs.list[[i]] = data.frame(sapply(p$classOutput, function(x) x$predicted[, 1]))
  }
  
  testSimpleParsets("multilabel.randomForestSRC", multilabel.df, multilabel.target, multilabel.train.inds, old.predicts.list, parset.list)
  testProbParsets ("multilabel.randomForestSRC", multilabel.df, multilabel.target, multilabel.train.inds, old.probs.list, parset.list)
})