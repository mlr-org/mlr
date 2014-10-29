makeNoFeaturesModel = function(targets, task.desc) {
  setClasses(list(targets = targets, task.desc = task.desc),
    "NoFeaturesModel")
}


predict_nofeatures = function(model, newdata) {
  lrn = getLearnerModel(model)
  y = lrn$targets
  type = lrn$type
  # for regression return constant mean
  if (type == "regr") {
    return(rep(mean(y), nrow(newdata)))
  }
  if (type == "classif") {
    tab = prop.table(table(y))
    probs = as.numeric(tab)
    if (lrn$predict.type == "response")
      return(sample(as.factor(names(tab)), nrow(newdata), prob = probs, replace = TRUE))
    probs = t(replicate(nrow(newdata), probs))
    colnames(probs) = names(tab)
    return(probs)
  }
  if (type == "surv") {
    if (lrn$predict.type == "response")
      return(runif(nrow(y)))
    # FIXME: probs / brier for survival should use something like median survival time
  }
  stopf("NoFeaturesModel for learner type '%s' not implemented", type)
}
