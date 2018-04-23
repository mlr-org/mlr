context("FDA_classif_fgam")
test_that("fgam works for classifcation", {
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
  # prob output
  lrn = makeLearner("classif.fgam", par.vals = list(mgcv.te_ti.k = 3L, mgcv.te_ti.m = 2), predict.type = "prob")
  m = train(lrn, gunpoint.task)
  expect_true(TRUE)
})
