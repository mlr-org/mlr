# FIXME: remove or finish
#makeRLearner.classif.loclda = function() {
#  makeRLearnerClassif(
#    cl = "classif.loclda",
#    package = "klaR",
#    oneclass = FALSE,
#    twoclass = TRUE,
#    multiclass = TRUE,
#    missings = FALSE,
#    numerics = TRUE,
#    factors = TRUE,
#    prob = TRUE,
#    weights = FALSE
#  )
#}

#trainLearner.classif.loclda = function(.learner, .task, .subset,  ...) {
#  p = predict(.model$learner.model, newdata=.newdata, ...)
#  if(.learner$predict.type == "response")
#    return(p$class)
#  else
#    return(p$posterior)
#}