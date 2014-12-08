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
  attr(x, "mlr.train.info") %??% attr(x$learner.model, "mlr.train.info")
}

"%??%" = function(lhs, rhs) {
  if (missing(lhs) || is.null(lhs)) rhs else lhs
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
