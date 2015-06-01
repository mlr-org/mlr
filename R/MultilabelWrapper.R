#' @export
trainLearner.MultilabelWrapper <- function(.learner, .task, .subset, .weights =NULL,...)
{
  class(.learner) = class(.learner)[-1]
  train = list()
  data = getTaskData(.task)
  for(i in 1:length(.task$task.desc$target)){
    task = makeClassifTask(id = .learner$id, data = data[!colnames(data) == .task$task.desc$target[-i]], 
                           target = .task$task.desc$target[i])
    train[[i]] = train(.learner, task, subset = .subset, weights = .weights)
  }
  names(train) = .task$task.desc$target
  class(train) = c("MultilabelWrapper")
  return(train)
}

#' @export
predictLearner.MultilabelWrapper = function(.learner, .model, .newdata, ...) {
  pred = list()
  for (i in 1:length(.model$learner.model)){
    model = .model$learner.model[i][[1]]
    pred[[i]] = predict(object = model, newdata = .newdata)$data
    if (.learner$predict.type == "prob"){
      pred[[i]] = pred[[i]][-ncol(pred[[i]])]
      names(pred[[i]]) = substr(names(pred[[i]]),6,nchar(names(pred[[i]])))
    }
  }
    names(pred) = names(.model$learner.model)
  return(pred)
}
