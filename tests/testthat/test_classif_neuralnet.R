context("classif_neuralnet")

test_that("classif_neuralnet", {
  requirePackages("neuralnet", default.method = "load")
  
  set.seed(getOption("mlr.debug.seed"))
  capture.output({
    set.seed(getOption("mlr.debug.seed"))
    # neuralnet is not dealing with formula with `.` well
    nms = names(binaryclass.train)
    formula_head = as.character(binaryclass.formula)[2]
    varnames = nms[nms!=formula_head]
    formula_head = paste('as.numeric(',formula_head,')~')
    formula_expand = paste(formula_head, paste(varnames, collapse = "+"))
    formula_expand = as.formula(formula_expand)
    traindat = binaryclass.train
    traindat[[binaryclass.target]] = as.numeric(traindat[[binaryclass.target]])-1
    
    m = neuralnet::neuralnet(formula_expand, hidden=7, data=traindat, err.fct='ce')
    p = neuralnet::compute(m, covariate = binaryclass.test[,-ncol(binaryclass.test)])
  })
  
  # Neuralnet doesn't have the `predict` method
  set.seed(getOption("mlr.debug.seed"))
  lrn = makeLearner("classif.neuralnet",hidden=7)
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  m2 = try(train(lrn, task, subset = binaryclass.train.inds))
  p2 = predictLearner(.learner=lrn,.model=m2,
                      .newdata = binaryclass.test[,-ncol(binaryclass.test)])
  expect_equal(p,p2,tol=1e-4)
})
