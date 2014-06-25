makeNoFeaturesModel = function(targets, task.desc) {
  setClasses(list(targets = targets, task.desc = task.desc),
    "NoFeaturesModel")
}


predict_nofeatures = function(model, newdata) {
  y = getLearnerModel(model)$targets
  # for regression return constant mean
  if (model$learner$type == "regr")
    return(rep(mean(y), nrow(newdata)))
  tab = prop.table(table(y))
  probs = as.numeric(tab)
  if(model$learner$predict.type == "response")
    return(sample(as.factor(names(tab)), nrow(newdata), prob = probs, replace = TRUE))
  else {
    probs = t(replicate(nrow(newdata), probs))
    colnames(probs) = names(tab)
    return(probs)
  }
}

