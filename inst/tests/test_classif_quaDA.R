context("classif_quaDA")

test_that("classif_quaDA", {
  library(DiscriMiner)
	set.seed(getOption("mlr.debug.seed"))
  m = quaDA(multiclass.train[,-multiclass.class.col], group=multiclass.train[,multiclass.class.col])
  #m2 = quaDA(binaryclass.train[,1:10], group=binaryclass.train[,binaryclass.class.col], prob=TRUE)
	p =  classify(m, newdata=multiclass.test[,-multiclass.class.col])
  #p2 = classify(m2, newdata=binaryclass.test[,1:10])
	testSimple("classif.quaDA", multiclass.df, multiclass.target, multiclass.train.inds, p$pred_class)
	
  tt = function (formula, data, subset, ...) {
    j = which(colnames(data) == as.character(formula)[2])
    m = quaDA(variables = data[subset,-j], group = data[subset,j])
    list(model = m, target=j)
  }
  
  tp = function(model, newdata) {
    classify(model$model, newdata = newdata[,-model$target])$pred_class
  }
  
  testCV("classif.quaDA", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp)
})
