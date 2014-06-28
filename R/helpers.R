requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}

measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep = ".")
}

perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits = 3L), sep = ""), collapse = ",")
}

recodeY = function(y, type, positive) {
  # FIXME: support left, right and interval for surv
  switch(type,
    "no" = y,
    "01" = as.numeric(y == positive),
    "-1+1" = as.numeric(2L*(y == positive)-1L),
    "surv" = Surv(time = y[, 1L], event = y[, 2L], type = "right"),
    stop("Unknown value for 'type'"))
}


removeFromDots = function(ns, ...) {
  args = list(...)
  args[setdiff(names(args), ns)]
}


filterNull = function(x) {
  x[!vapply(x, is.null, logical(1L))]
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
