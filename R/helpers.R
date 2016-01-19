requireLearnerPackages = function(learner) {
  requirePackages(learner$package, why = paste("learner", learner$id), default.method = "load")
}

cleanupPackageNames = function(pkgs) {
  gsub("^[!_]", "", pkgs)
}

# paste together measure and aggregation ids
measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep = ".")
}

# paste together measure and aggregation names
measureAggrPrettyName = function(measure) {
  paste(measure$name, measure$aggr$name, sep = ": ")
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

getSupportedTaskTypes = function() {
  c("classif", "regr", "surv", "costsens", "cluster", "multilabel")
}

# find duplicate measure names or ids and paste together those
# with the associated aggregation ids or names
replaceDupeMeasureNames = function(measures, x = "id") {
  assertList(measures, "Measure")
  assertChoice(x, c("id", "name"))
  meas.names = extractSubList(measures, x)
  dupes = table(meas.names)
  dupes = which(meas.names %in% names(dupes[dupes > 1]))
  if (x == "id")
    new.names = sapply(measures[dupes], function(x) measureAggrName(x))
  else
    new.names = sapply(measures[dupes], function(x) measureAggrPrettyName(x))
  meas.names[dupes] = new.names
  unlist(meas.names)
}
