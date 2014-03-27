context("classif_plr")

test_that("classif_plr", {
  library(stepPlr)
  
  mydata = binaryclass.train
  mydata[, binaryclass.target] = as.numeric(mydata[, binaryclass.target] ==  binaryclass.task$task.desc$positive)
  m = plr(x=mydata[, -binaryclass.class.col], y=mydata[, binaryclass.target], cp = 2)
  p = predict(m, newx=binaryclass.test[, -binaryclass.class.col], type="response")
  old.probs = p
  p = as.factor(ifelse(p > 0.5, "M", "R"))
  old.predicts = p
  
  testSimple("classif.plr", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts)
  testProb("classif.plr", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs)
  
})
