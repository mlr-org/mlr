context("classif_neuralnet")

test_that("classif_neuralnet", {
  requirePackagesOrSkip("neuralnet", default.method = "load")

  # test with empty paramset
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    nms = names(binaryclass.train)
    formula.head = as.character(binaryclass.formula)[2]
    varnames = nms[nms != formula.head]
    formula.head = paste("as.numeric(", formula.head, ")~")
    formula.expand = paste(formula.head, paste(varnames, collapse = "+"))
    formula.expand = as.formula(formula.expand)
    traindat = binaryclass.train
    traindat[[binaryclass.target]] = as.numeric(traindat[[binaryclass.target]]) - 1

    m = neuralnet::neuralnet(formula.expand, data = traindat, err.fct = "ce",
      linear.output = FALSE)
    p = neuralnet::compute(m,
      covariate = binaryclass.test[, -ncol(binaryclass.test)])
    p = as.numeric(as.vector(p[[2]]) > 0.5)
    p = factor(p, labels = binaryclass.class.levs)
  })

  testSimple("classif.neuralnet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p,
    parset = list())

  # test with params passed
  capture.output({
    # neuralnet is not dealing with formula with `.` well
    nms = names(binaryclass.train)
    formula.head = as.character(binaryclass.formula)[2]
    varnames = nms[nms != formula.head]
    formula.head = paste("as.numeric(", formula.head, ")~")
    formula.expand = paste(formula.head, paste(varnames, collapse = "+"))
    formula.expand = as.formula(formula.expand)
    traindat = binaryclass.train
    traindat[[binaryclass.target]] = as.numeric(traindat[[binaryclass.target]]) - 1

    m = neuralnet::neuralnet(formula.expand, hidden = 7, data = traindat,
      err.fct = "ce", linear.output = FALSE)
    p = neuralnet::compute(m,
      covariate = binaryclass.test[, -ncol(binaryclass.test)])
    p = as.numeric(as.vector(p[[2]]) > 0.5)
    p = factor(p, labels = binaryclass.class.levs)
  })

  testSimple("classif.neuralnet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, p, parset = list(hidden = 7, err.fct = "ce"))

  # Neuralnet doesn't have the `predict` method
  #   set.seed(getOption("mlr.debug.seed"))
  #   lrn = makeLearner("classif.neuralnet",hidden=7)
  #   task = makeClassifTask(data = binaryclass.df, target = binaryclass.target)
  #   m2 = try(train(lrn, task, subset = binaryclass.train.inds))
  #   p2 = predictLearner(.learner=lrn,.model=m2,
  #                       .newdata = binaryclass.test[,-ncol(binaryclass.test)])
  #   expect_equal(p,p2,tol=1e-4)
})
