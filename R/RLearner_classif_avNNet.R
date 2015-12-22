#' @export
makeRLearner.classif.avNNet = function() {
  makeRLearnerClassif(
    cl = "classif.avNNet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "repeats", default = 5L, lower = 1L),
      makeLogicalLearnerParam(id = "bag", default = FALSE),
      makeIntegerLearnerParam(id = "size", default = 3L, lower = 0L),
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      makeLogicalLearnerParam(id = "skip", default = FALSE),
      makeNumericLearnerParam(id = "rang", default = 0.7),
      makeNumericLearnerParam(id = "decay", default = 0),
      makeLogicalLearnerParam(id = "Hess", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE),
      makeIntegerLearnerParam(id = "MaxNWts", default = 1000L, lower = 1L),
      makeNumericLearnerParam(id = "abstoll", default = 1.0e-4),
      makeNumericLearnerParam(id = "reltoll", default = 1.0e-8)
    ),
    par.vals = list(size = 3L),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "weights"),
    name = "Neural Network",
    short.name = "avNNet",
    note = "`size` has been set to `3` by default. Doing bagging training of `nnet` if set `bag = TRUE`."
  )
}

#' @export
trainLearner.classif.avNNet = function(.learner, .task, .subset, .weights = NULL, ...) {
  repeats = 5
  bag = FALSE
  
  nms = names(.learner$par.vals)
  ind = grep('repeats',nms)
  if (length(ind)>0)
    repeats = .learner$par.vals[[ind]]
  ind = grep('bag',nms)
  if (length(ind)>0)
    bag = .learner$par.vals[[ind]]
  
  assertInt(repeats, lower = 1)
  assertFlag(bag)
  
  nets = vector(repeats,mode='list')
  dat = getTaskData(.task, .subset)
  assertInt(nrow(dat), lower = 1)
  ind = 1:nrow(dat)
  
  for (i in 1:repeats){
    if (bag) {
      ind = sample(1:nrow(dat))
    }
    assertInteger(ind, len = nrow(dat))
    if (is.null(.weights)) {
      f = getTaskFormula(.task)
      nets[[i]] = nnet::nnet(f, data = dat[ind,], ...)
    } else {
      f = getTaskFormula(.task)
      nets[[i]] = nnet::nnet(f, data = dat[ind,], weights = .weights, ...)
    }
  }
  return(nets)
}

#' @export
predictLearner.classif.avNNet = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, response = "class", prob = "raw")
  repeats = length(.model$learner.model)
  
  assertInt(repeats, lower = 1)
  
  pred = 0
  for (i in 1:repeats){
    pred = pred+predict(.model$learner.model[[i]], newdata = .newdata, 
                        type = "raw", ...)
  }
  pred = pred/repeats
  if (length(.model$task.desc$class.levels) == 2L) {
    pred = cbind(1-pred, pred)
    colnames(pred) = .model$learner.model[[1]]$lev
  }
  if (type == "class") {
    classes <- colnames(pred)[max.col(pred)]
    return(as.factor(classes))
  } else {
    return(pred)
  }
  return(pred)
}
