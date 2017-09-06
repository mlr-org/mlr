# Be carefull that one need to change the columns name again after cbine several matrix
context("FDA_regr_fgam")
test_that("FDA_regr_fgam", {
  requirePackagesOrSkip("refund")
  data(DTI)
  DTI1 = DTI[DTI$visit == 1 & complete.cases(DTI),]  # subtract a portion of the data
  # DTI1 is already a matrix dataframe, Fit model with additive functional term for functional feature cca, using tensor product basis
  fit.af = refund::pfr(formula = pasat ~ af(cca, Qtransform=TRUE, k=c(7,7)), data = DTI1)
  predict(fit.af, newdata = DTI1, type = 'response')
  df = data.frame(as.list(DTI[, c("pasat", "cca")]))
  df = df[!is.na(df$pasat),]
  df = impute(df, classes = list(numeric = imputeMedian()))$data
  # impute those na values
  fdf = makeFunctionalData(df, fd.features = list("cca" = 2:94))
  lrn = makeLearner("regr.fdafgam", mgcv.s.k = -1L )
  task = makeRegrTask(data = fdf, target = "pasat")
  mod1f = train(learner = lrn, task = task)
  predict(object = mod1f, newdata = fdf)
})
