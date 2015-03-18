context("classif_gam")

test_that("classif_gam", {
  requirePackages("mgcv", default.method = "load")
  
  m = mgcv::gam(formula = getTaskFormula(binaryclass.task, explicit.features = TRUE), data = binaryclass.train, control = learnerArgsToControl(mgcv::gam.control), family = binomial)
  p = predict(m, newdata = binaryclass.test, type = "response")
  p.prob = 1-p
  p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
  
  testSimple("classif.gam", binaryclass.df, binaryclass.target, binaryclass.train.inds, p.class)
  
})