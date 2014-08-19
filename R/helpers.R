requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}

measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep = ".")
}

perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits = 3L), sep = ""), collapse = ",")
}

removeFromDots = function(ns, ...) {
  args = list(...)
  args[setdiff(names(args), ns)]
}


filterNull = function(x) {
  x[!vlapply(x, is.null)]
}

attachTrainingInfo = function(x, info) {
  attr(x, "mlr.train.info") = info
  x
}

getTrainingInfo = function(x) {
  attr(x, "mlr.train.info")
}

isNotSet = function(x) {
  missing(x) || is.null(x)
}

isSet = function(x) {
  !isNotSet(x)
}
