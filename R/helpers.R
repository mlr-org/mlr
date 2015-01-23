requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id), default.method = "load")
}

cleanupPackageNames = function(pkgs) {
  gsub("^[!_]", "", pkgs)
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

attachTrainingInfo = function(x, info) {
  attr(x, "mlr.train.info") = info
  x
}

getTrainingInfo = function(x) {
  attr(x, "mlr.train.info") %??% attr(x$learner.model, "mlr.train.info")
}

"%??%" = function(lhs, rhs) {
  if (missing(lhs) || is.null(lhs)) rhs else lhs
}

getLearnerOptions = function(lrn, opts) {
  lrn.opts = getLeafLearner(lrn)$config
  setNames(lapply(opts, function(x) lrn.opts[[x]] %??% getMlrOption(x)), opts)
}

# p = probabilites for levs[2] => matrix with probs for levs[1] and levs[2]
propVectorToMatrix = function(p, levs) {
  assertNumeric(p)
  y = matrix(0, ncol = 2L, nrow = length(p))
  colnames(y) = levs
  y[, 2L] = p
  y[, 1L] = 1-p
  y
}

getSupportedLearnerProperties = function(type = NA_character_) {
  p = list(
    classif  = c("numerics", "factors", "ordered", "missings", "weights", "prob", "twoclass", "multiclass"),
    regr     = c("numerics", "factors", "ordered", "missings", "weights", "se"),
    cluster  = c("numerics", "factors", "ordered", "missings", "weights", "prob"),
    surv     = c("numerics", "factors", "ordered", "missings", "weights", "prob", "rcens"),
    costsens = c("numerics", "factors", "ordered", "missings", "weights", "prob", "twoclass", "multiclass")
  )
  if (is.na(type))
    unique(unlist(p))
  else
    p[[type]]
}
