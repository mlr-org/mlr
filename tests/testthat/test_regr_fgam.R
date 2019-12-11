# Be careful that one need to change the columns name again after cbine several
# matrix
context("FDA_regr_fgam")

test_that("testif FDA_regr_fgam generate same prediction with refund::pfr", {
  requirePackagesOrSkip("refund")

  data(DTI)
  dti1 = DTI[DTI$visit == 1 & complete.cases(DTI), ]
  # subset a portion of the data(complete.cases select non missing value rows)
  # dti1 is already a matrix dataframe
  fit.af = pfr(formula = pasat ~ af(cca, Qtransform = TRUE, k = 7, m = 2),
    data = dti1)
  prd.refund = predict(fit.af, newdata = dti1, type = "response")
  # makeFunctionalData require plain dataframe
  df = data.frame(as.list(dti1[, c("pasat", "cca")]))
  fdf = makeFunctionalData(df, fd.features = list("cca" = 2:94))
  lrn = makeLearner("regr.fgam", Qtransform = TRUE, mgcv.te_ti.k = 7,
    mgcv.te_ti.m = 2)
  task = makeRegrTask(data = fdf, target = "pasat")
  mod1f = train(learner = lrn, task = task)
  prd.mlr = predict(object = mod1f, newdata = fdf)
  res = all.equal(as.numeric(prd.refund), prd.mlr$data$response)
  expect_true(res)
})
