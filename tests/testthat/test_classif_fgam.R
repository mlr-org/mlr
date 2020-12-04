
test_that("fgam works for classifcation", {
  # errors on R 4.1
  skip_if(sessionInfo()$R.version$status == "Under development (unstable)")
  requirePackagesOrSkip("refund")
  dd = getTaskData(gunpoint.task, functionals.as = "matrix", target.extra = TRUE)
  matdd = list()
  matdd$fd = dd$data$fd
  hh = getBinomialTarget(gunpoint.task)
  matdd$X1 = hh$newtarget
  fit.af = pfr(formula = X1 ~ af(fd, Qtransform = TRUE, k = 3, m = 2), data = matdd, family = binomial())
  lrn = makeLearner("classif.fgam", par.vals = list(mgcv.te_ti.k = 3L, mgcv.te_ti.m = 2))
  m = train(lrn, gunpoint.task)
  cp = predict(m, task = gunpoint.task)
  expect_class(cp, "Prediction")

  # prob output
  lrn = makeLearner("classif.fgam", par.vals = list(mgcv.te_ti.k = 3L, mgcv.te_ti.m = 2), predict.type = "prob")
  m2 = train(lrn, gunpoint.task)
  cp2 = predict(m2, task = gunpoint.task)
  expect_class(cp2, "Prediction")
})
