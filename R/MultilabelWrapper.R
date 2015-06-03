
#' @export
makeMultilabelWrapper = function(learner) {
  learner = checkLearner(learner, type=c("classif"))
  if (!(learner$predict.type %in% c("response", "prob")))
    stop("Predict type of the basic learner must be 'response' or 'prob'.")
  id = paste("multilabel",learner$id, sep = ".")
  packs = learner$package
  x = makeHomogeneousEnsemble(id, learner$type, learner, packs, 
                              learner.subclass = "MultilabelWrapper", model.subclass = "MultilabelModel")
  return(x)
}


#' @export
trainLearner.MultilabelWrapper <- function(.learner, .task, .subset, .weights =NULL,...)
{
  train = list()
  data = getTaskData(.task)
  for (i in 1:length(.task$task.desc$target)){
    task = makeClassifTask(id = .learner$id, data = data[!colnames(data) == .task$task.desc$target[-i]], 
                           target = .task$task.desc$target[i])
    task = subsetTask(task, subset = .subset)
    train[[i]] = train(.learner$next.learner, task, weights = .weights)
  }
  names(train) = .task$task.desc$target
  class(train) = c("MultilabelWrapper")
  return(train)
}

#' @export
print.MultilabelModel = function(x, ...) {
  cat(
    "Model for id = ", x$learner$id, " class = ", getClass1(x$learner), "\n",
    "Trained on obs: ", length(x$subset), "\n",
    "Used features: ", length(x$features), "\n",
    "Hyperparameters: ", getHyperParsString(x$learner), "\n",
    sep = ""
  )
}

#' @export
predictLearner.MultilabelWrapper = function(.learner, .model, .newdata, ...) {
  pred = list()
  for (i in 1:length(.model$learner.model)){
    model = .model$learner.model[[i]]
    pred[[i]] = predict(object = model, newdata = .newdata)$data
    if (.learner$predict.type == "prob"){
      pred[[i]] = pred[[i]][-ncol(pred[[i]])]
      names(pred[[i]]) = substr(names(pred[[i]]),6,nchar(names(pred[[i]])))
    }
  }
  names(pred) = names(.model$learner.model)
  return(pred)
}
