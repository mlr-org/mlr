context("surv_penalized_ridge")

test_that("surv_penalized_ridge", {
  requirePackages("survival", default.method = "load")
  requirePackages("penalized", default.method = "load")
  parset.list = list(
    list(lambda2 = 1),
    list(lambda2 = 2, maxiter = 15L)
  )

  old.predicts.list = list()
  
  # survival learners did not work for factors
  surv.df$fac = factor(sample(c("a", "b"), nrow(surv.df), replace = TRUE,
    prob = c(0.2, 0.8)))
  surv.train = surv.df[surv.train.inds, ]
  surv.test = surv.df[surv.test.inds, ]
  surv.train.feats = surv.train[, names(surv.train) %nin% surv.target]
  surv.train.feats = as.matrix(createDummyFeatures(surv.train.feats))
  surv.test.feats = surv.test[, names(surv.test) %nin% surv.target]
  surv.test.feats = as.matrix(createDummyFeatures(surv.test.feats))
  for (i in 1:length(parset.list)) {
    pars = c(list(response = Surv(surv.train$time, surv.train$status, type = "right"),
      penalized = surv.train.feats,
      model = "cox", trace = FALSE, fusedl = FALSE), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(penalized::penalized, pars)
    p = penalized::survival(penalized::predict(m,
      penalized = surv.test.feats), Inf)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("surv.penalized.ridge", surv.df, surv.target,
    surv.train.inds, old.predicts.list, parset.list)
})
