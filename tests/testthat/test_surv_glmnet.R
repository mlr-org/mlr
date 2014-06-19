context("surv_glmnet")

test_that("surv_glmnet", {
  library(survival)
  library(glmnet)
  parset.list = list(
    list(),
    list(nfolds = 5, alpha = 0.5),
    list(alpha = 0.3),
    list(nfolds = 10, alpha = 0)
  )
  
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    y = as.matrix(surv.train[, surv.target])
    colnames(y) = c("time", "status")
    pars = c(list(y = Surv(time=surv.train[, "time"], event=surv.train[, "event"]), 
      x = as.matrix(surv.train[, -c(1,2,7)]), family = "cox"), parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(cv.glmnet, pars)
    p  = predict(m, newx = as.matrix(surv.test[, -c(1,2,7)]), type = "link", s = m$lambda.min)
    old.predicts.list[[i]] = as.numeric(p)
  }
  
  testSimpleParsets("surv.glmnet", surv.df[, -7], surv.target, surv.train.inds, old.predicts.list, parset.list)
})
