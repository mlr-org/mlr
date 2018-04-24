context("classif_gbm")

test_that("classif_gbm", {
  requirePackagesOrSkip("gbm", default.method = "load")

  parset.list = list(
    list(),
    list(n.trees = 600),
    list(interaction.depth = 2)
  )

  old.predicts.list = list()
  old.probs.list = list()

  mydata = binaryclass.train
  mydata[, binaryclass.target] = as.numeric(mydata[, binaryclass.target] ==  getTaskDesc(binaryclass.task)$positive)
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(binaryclass.formula, data = mydata, distribution = "bernoulli")
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(gbm::gbm, pars)
    })
    set.seed(getOption("mlr.debug.seed"))
    p = gbm::predict.gbm(m, newdata = binaryclass.test, n.trees = length(m$trees), type = "response")
    old.probs.list[[i]] = p
    p = as.factor(ifelse(p > 0.5, "M", "R"))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("classif.gbm", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.gbm", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)

  set.seed(getOption("mlr.debug.seed"))
  m = gbm::gbm(multiclass.formula, data = multiclass.train, n.trees = 300, interaction.depth = 2, distribution = "multinomial")
  p = gbm::predict.gbm(m, newdata = multiclass.test, n.trees = 300)
  y = factor(apply(p[, , 1], 1, function(r) colnames(p)[which.max(r)]))
  testSimple("classif.gbm", multiclass.df, multiclass.target, multiclass.train.inds, y,
    parset = list(n.trees = 300, interaction.depth = 2, distribution = "multinomial"))
})

test_that("classif_gbm keep.data is passed correctly", {
  train(makeLearner("classif.gbm", keep.data = FALSE), binaryclass.task)
  train(makeLearner("classif.gbm", keep.data = TRUE), binaryclass.task)
})
