makeNoFeaturesModel = function(targets, task.desc) {
  setClasses(list(targets = targets, task.desc = task.desc),
    "NoFeaturesModel")
}


predictNofeatures = function(model, newdata) {
  y = getLearnerModel(model)$targets
  type = model$learner$type
  # for regression return constant mean
  if (type == "regr") {
    return(rep(mean(y), nrow(newdata)))
  }
  if (type == "classif") {
    tab = prop.table(table(y))
    probs = as.numeric(tab)
    if (model$learner$predict.type == "response") {
      return(sample(as.factor(names(tab)), nrow(newdata), prob = probs, replace = TRUE))
    }
    probs = t(replicate(nrow(newdata), probs))
    colnames(probs) = names(tab)
    return(probs)
  }
  if (type == "surv") {
    if (model$learner$predict.type == "response") {
      return(runif(nrow(newdata)))
    }
    # FIXME: probs / brier for survival should use something like median survival time
  }
  stopf("NoFeaturesModel for learner type '%s' not implemented", type)
}
