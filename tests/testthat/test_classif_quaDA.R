context("classif_quaDA")

test_that("classif_quaDA", {
  requirePackagesOrSkip("DiscriMiner", default.method = "load")

  m = DiscriMiner::quaDA(multiclass.train[, -multiclass.class.col],
    group = multiclass.train[, multiclass.class.col])
  p = DiscriMiner::classify(m, newdata = multiclass.test[, -multiclass.class.col])
  testSimple("classif.quaDA", multiclass.df, multiclass.target,
    multiclass.train.inds, p$pred_class)

  tt = function(formula, data, subset, ...) {
    j = which(colnames(data) == as.character(formula)[2])
    m = DiscriMiner::quaDA(variables = data[subset, -j], group = data[subset, j])
    list(model = m, target = j)
  }

  tp = function(model, newdata) {
    DiscriMiner::classify(model$model,
      newdata = newdata[, -model$target])$pred_class
  }

  testCV("classif.quaDA", multiclass.df, multiclass.target, tune.train = tt,
    tune.predict = tp)
})
