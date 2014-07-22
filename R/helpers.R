requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}

measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep = ".")
}

perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits = 3L), sep = ""), collapse = ",")
}

recodeInterval2Data = function(y, type) {
  if (type == "left") 
    ind = 2L
  else ind = 1L
  time = y[, ind]
  event = as.integer(y[, -ind] == time)
  return(Surv(time = time, event = event, type = type))
}

recodeY = function(y, type, positive) {
  switch(type,
    "no" = y,
    "01" = as.numeric(y == positive),
    "-1+1" = as.numeric(2L*(y == positive)-1L),
    "interval" = Surv(time = y[, 1L], time2 = y[, 2L], type = "interval2"),
    "left" = recodeInterval2Data(y, "left"),
    "right" = recodeInterval2Data(y, "right"),
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
