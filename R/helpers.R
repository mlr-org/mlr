requireLearnerPackages = function(learner) {
  requirePackages(learner$package, why = stri_paste("learner", learner$id, sep = " "), default.method = "load")
}

cleanupPackageNames = function(pkgs) {
  stri_replace_all(pkgs, "", regex = "^[!_]")
}

# paste together measure and aggregation ids
measureAggrName = function(measure) {
  stri_paste(measure$id, measure$aggr$id, sep = ".")
}

# paste together measure and aggregation names
measureAggrPrettyName = function(measure) {
  stri_paste(measure$name, measure$aggr$name, sep = ": ")
}

# convert a named numvec of perf values (think 'aggr' from resample) into flat string
# ala <name><sep><value>,...,<name><sep><value>
perfsToString = function(y, sep = "=", digits = options()$digits) {
  stri_paste(stri_paste(names(y), "=", formatC(y, digits = digits,
    flag = "0", format = "f"), sep = ""), collapse = ",", sep = " ")
}

# Used for the resample output logging lines:
# Formats and joins the string 'prefix' and the vector 'y' to obtain an aligned output line
# If y is numeric we trim to desired digit with
# if not it's a character and we only need to take care that the col has desired width
# Example output (prefix = "[Resample] iter 1:"):
# [Resample] iter 1:    0.0000000    0.0370370    0.9629630
printResampleFormatLine = function(prefix, y, digits = options()$digits) {

  # get desired width for each col (if measure ids are short --> digits)
  # +3L to obtain spaces between cols
  if (is.null(names(y))) {
    names(y) = y
  }
  tab.width = max(stri_width(names(y)), digits) + 3L
  # if we get perf vals format decimals and add trailing zeros where needed
  if (is.numeric(y)) {
    y = formatC(y, digits = digits, flag = "0", format = "f")
  }
  # Extend witdh of prefix and y. width = 22 is the ideal size for
  # the prefix column. Change value here when iter.message was
  # modified in resample.R
  prefix = formatC(prefix, width = 22, flag = "-")
  str = stri_flatten(formatC(y, width = tab.width, flag = "-"))

  message(stri_paste(prefix, str, collapse = " "))
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
  y[, 1L] = 1 - p
  y
}

#' @title List the supported task types in mlr
#'
#' @description
#' Returns a character vector with each of the supported task types in mlr.
#'
#' @return ([character]).
#' @export
listTaskTypes = function() {
  c("classif", "regr", "surv", "costsens", "cluster", "multilabel")
}

# Maybe move to BBmisc at some point
measureTime = function(expr, ee = parent.frame()) {
  before = proc.time()[[3L]]
  force(expr)
  proc.time()[[3L]] - before
}

# find duplicate measure names or ids and paste together those
# with the associated aggregation ids or names
replaceDupeMeasureNames = function(measures, x = "id") {
  assertList(measures, "Measure")
  assertChoice(x, c("id", "name"))
  meas.names = extractSubList(measures, x)
  dupes = table(meas.names)
  dupes = which(meas.names %in% names(dupes[dupes > 1]))
  if (x == "id") {
    new.names = sapply(measures[dupes], function(x) measureAggrName(x))
  } else {
    new.names = sapply(measures[dupes], function(x) measureAggrPrettyName(x))
  }
  meas.names[dupes] = new.names
  unlist(meas.names)
}

# suppresses a warning iff the warning message contains the
# substring `str`.
suppressWarning = function(expr, str) {
  withCallingHandlers(expr, warning = function(w) {
    if (stri_detect_fixed(stri_flatten(w$message), str)) {
      invokeRestart("muffleWarning")
    }
  })
}

hasEmptyLevels = function(x) {
  !all(levels(x) %chin% as.character(unique(x)))
}

# thin a vector
thin = function(x, skip = 0) {
  n = length(x)
  x[seq(1, n, by = skip)]
}

# scale window if < 1
scaleWindows = function(window, scaler) {
  if (window < 1) {
    scaled.window = round(window * scaler)
  } else {
    scaled.window = round(window)
  }
  return(scaled.window)
}

# Create the resampling windows for growing and fixed window cross validation
makeResamplingWindow = function(desc, size, task = NULL, coords, window.type) {

  initial.window.abs = scaleWindows(desc$initial.window, size)
  horizon.window = scaleWindows(desc$horizon, initial.window.abs)

  if (size - initial.window.abs < horizon.window) {
    stop(catf("The initial window is %i observations while the data is %i observations. \n There is not enough data left (%i observations) to create a test set for a %i size horizon.",
      initial.window.abs, size, initial.window.abs - size, horizon.window))
  }
  if (window.type == "FixedWindowCV") {
    stops = (seq(size))[initial.window.abs:(size - horizon.window)]
    starts = stops - initial.window.abs + 1
    train.inds = mapply(seq, starts, stops, SIMPLIFY = FALSE)
    test.inds = mapply(seq, stops + 1, stops + horizon.window, SIMPLIFY = FALSE)
  } else if (window.type == "GrowingWindowCV") {
    stops = (seq(from = 1, to = size))[initial.window.abs:(size - horizon.window)]
    starts = rep(1, length(stops))
    train.inds = mapply(seq, starts, stops, SIMPLIFY = FALSE)
    test.inds = mapply(seq, stops + 1, stops + horizon.window, SIMPLIFY = FALSE)
  }
  skip = scaleWindows(desc$skip, length(train.inds))

  if (skip > 0) {
    train.inds = thin(train.inds, skip = skip)
    test.inds = thin(test.inds, skip = skip)
  }
  if (length(test.inds) == 0) {
    stop("Skip is too large and has removed all resampling instances. Please lower the value of skip.")
  }
  if (test.inds[[length(test.inds)]][horizon.window] != size) {
    num.excluded = size - test.inds[[length(test.inds)]][horizon.window]
    warning(paste0("The last ", num.excluded, " observation(s) were excluded. To include these observations please change either the initial.window or horizon."))
  }
  desc$iters = length(test.inds)
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds)
}
