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

attachTrainingInfo = function(x, info) {
  attr(x, "mlr.train.info") = info
  x
}

getTrainingInfo = function(x) {
  attr(x, "mlr.train.info")
}

"%??%" = function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

getLearnerOptions = function(lrn, opts) {
  lrn.opts = getLeafLearner(lrn)$config
  setNames(lapply(opts, function(x) lrn.opts[[x]] %??% getMlrOption(x)), opts)
}

# FIXME: remove after BBmisc update to 1.9
requirePackages = function(packs, why = NULL, stop = TRUE, suppress.warnings = FALSE, suppress.startup = TRUE, ...) {
  getSuppressor = function(suppress.warnings, suppress.startup) {
    if (suppress.warnings) {
      if (suppress.startup)
        return(function(expr) suppressPackageStartupMessages(suppressWarnings(expr)))
      return(suppressWarnings)
    }
    if (suppress.startup)
      return(suppressPackageStartupMessages)
    return(identity)
  }
  args = list(...)
  args$character.only = TRUE
  suppressor = getSuppressor(suppress.warnings, suppress.startup)
  packs.ok = sapply(packs, function(x) {
    args$package = x
    suppressor(do.call(require, args))
  })
  if(stop && !all(packs.ok)) {
    ps = collapse(packs[!packs.ok])
    if (is.null(why))
      stopf("Please install the following packages: %s", ps)
    else
      stopf("For %s please install the following packages: %s", why, ps)
  }
  return(packs.ok)
}
