# Be carefull that one need to change the columns name again after cbine several matrix
context("FDA_regr_fgam")
test_that("testif FDA_regr_fgam generate same prediction with refund::pfr", {
  requirePackagesOrSkip("refund")
  data(DTI)
  DTI1 = DTI[DTI$visit == 1 & complete.cases(DTI),]  # subset a portion of the data(complete.cases select non missing value rows)
  # DTI1 is already a matrix dataframe, Fit model with additive functional term for functional feature cca, using tensor product basis
  fit.af = refund::pfr(formula = pasat ~ af(cca, Qtransform = TRUE, k = 7, m = 2), data = DTI1)
  prd_refund = predict(fit.af, newdata = DTI1, type = 'response')
  df = data.frame(as.list(DTI1[, c("pasat", "cca")]))  # makeFunctionalData require plain dataframe
  fdf = makeFunctionalData(df, fd.features = list("cca" = 2:94))  # dim(DTI1$cca) = (66,93)
  lrn = makeLearner("regr.fgam", Qtransform = TRUE, mgcv.te_ti.k = 7, mgcv.te_ti.m = 2)
  task = makeRegrTask(data = fdf, target = "pasat")
  mod1f = train(learner = lrn, task = task)
  prd_mlr = predict(object = mod1f, newdata = fdf)
  all.equal(as.numeric(prd_refund), prd_mlr$data$response)
})

test_that("fgam works for clasisifcation", {
  lrn = makeLearner("classif.fgam", par.vals = list(mgcv.te_ti.k = 7L, mgcv.te_ti.m = 2))
  m = train(lrn, gunpoint.task)
  cp = predict(m, task = gunpoint.task)
})
