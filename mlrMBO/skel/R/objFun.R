objFun = function(task, ...) {
  library(mlr)

  # some quick sanity checks 
  ddd = list(...)
  stopifnot(inherits(task, "ClassifTask"))
  stopifnot("model" %in% names(ddd))

  # extract model, filter and rename pars
  model = ddd$model
  ddd$model = NULL
  ddd = Filter(Negate(is.na), ddd)
  pat = sprintf("^%s\\.", model)
  if (!all(grepl(pat, names(ddd))))
    stop("Some parameters not intended for model")
  names(ddd) = sub(pat, "", names(ddd))

  rd = makeResampleDesc("CV", iters = 10L)
  learner = setHyperPars(makeLearner(sprintf("classif.%s", model)), par.vals = ddd)
  res = resample(learner = learner, task = task, resampling = rd)
  res$aggr[[1L]]
}

if (FALSE) { # code for testing
  library(BBmisc)
  library(mlr)
  source("paramSet.R")
  task = makeClassifTask(data = iris, target = "Species")
  des = generateDesign(10, getParamSet())

  for (i in seq_row(des)) {
    do.call(doTrainTest, c(list(task = task), as.list(des[i, ]))) 
    doTrainTest(task, model = "randomForest")
  }
}
