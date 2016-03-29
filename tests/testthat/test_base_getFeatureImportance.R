context("getFeatureImportance")

test_that("getFeatureImportance", {

  checkFeatureImportance = function(lrn.id) {
    par.vals = list()
    lrn.id.split = unlist(strsplit(lrn.id, split = ".", fixed = TRUE))
    type = lrn.id.split[1L]
    alg = lrn.id.split[2L]
    if (type == "classif") {
      tsk = binaryclass.task
    } else {
      if (type == "regr") {
        tsk = regr.task
      } else {
        if (type == "surv") {
          tsk = surv.task
        } else {
          stop("should not happen")
        }
      } 
    }

    # some learners need special param settings to compute variable importance
    # add them here if you implement a measure that requires that.
    # you may also want to change the params for the learner if training takes
    # a long time
    if (alg == "ranger")
      par.vals$importance = "impurity"
    if (alg == "boosting")
      par.vals$mfinal = 5L
    if (alg == "cforest")
      par.vals$ntree = 5L
    if (alg == "randomForestSRC")
      par.vals$ntree = 5L

    lrn = makeLearner(lrn.id, par.vals = par.vals)
    mod = train(lrn, tsk)
    feat.imp = getFeatureImportance(mod)
    expect_is(feat.imp, "numeric")
    expect_equal(names(feat.imp), mod$features)
  }

  feat.imp.methods = ls(getNamespace("mlr"), all.names = TRUE,
    pattern = "getFeatureImportance\\.")
  lrn.ids = gsub("getFeatureImportance.","",feat.imp.methods, fixed = TRUE)
  sapply(lrn.ids, checkFeatureImportance)
})
