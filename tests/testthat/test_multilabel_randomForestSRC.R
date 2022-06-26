
test_that("multilabel_randomForestSRC", {
  requirePackagesOrSkip("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 100),
    list(ntree = 250, mtry = 3),
    list(ntree = 250, nodesize = 2, na.action = "na.impute",
      importance = "permute", proximity = FALSE)
  )
  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = multilabel.train,
      formula = multilabel.formula.cbind, forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrc, parset)
    p = predict(m, newdata = multilabel.test, membership = FALSE,
      na.action = "na.impute")
    old.predicts.list[[i]] = as.data.frame(lapply(p$regrOutput,
    function(x) as.logical(round(x$predicted))))
    old.probs.list[[i]] = as.data.frame(lapply(p$regrOutput,
    function(x) x$predicted))
  }
  testSimpleParsets("multilabel.randomForestSRC", multilabel.df,
    multilabel.target, multilabel.train.inds, old.predicts.list, parset.list)
  testProbParsets("multilabel.randomForestSRC", multilabel.df,
    multilabel.target, multilabel.train.inds, old.probs.list, parset.list)
})
