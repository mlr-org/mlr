makeResampleDescHoldout = function(iters, split = 2/3) {
  assertNumeric(split, len = 1L, any.missing = FALSE, lower = 0, upper = 1)
  makeResampleDescInternal("holdout", iters = 1L, split = split)
}

makeResampleDescCV = function(iters = 10L) {
  iters = convertInteger(iters)
  assertInteger(iters, len = 1L, any.missing = FALSE, lower = 1L)
  makeResampleDescInternal("cross-validation", iters = iters)
}

makeResampleDescLOO = function() {
  makeResampleDescInternal("LOO", iters = NA_integer_)
}

makeResampleDescSubsample = function(iters = 30L, split = 2/3) {
  iters = convertInteger(iters)
  assertInteger(iters, len = 1L, any.missing = FALSE, lower = 1L)
  assertNumeric(split, len = 1L, any.missing = FALSE, lower = 0, upper = 1)
  makeResampleDescInternal("subsampling", iters = iters, split = split)
}

makeResampleDescBootstrap = function(iters = 30L) {
  iters = convertInteger(iters)
  assertInteger(iters, len = 1L, any.missing = FALSE, lower = 1L)
  makeResampleDescInternal("OOB bootstrapping", iters = iters)
}

makeResampleDescRepCV = function(reps = 10L, folds = 10L) {
  reps = convertInteger(reps)
  assertInteger(reps, len = 1L, any.missing = FALSE, lower = 2L)
  folds = convertInteger(folds)
  assertInteger(folds, len = 1L, any.missing = FALSE, lower = 2L)
  makeResampleDescInternal("repeated cross-validation", iters = folds*reps, folds = folds, reps = reps)
}

#' @export
print.HoldoutDesc = function(x, ...) {
  catf("Resample description: %s with %.2f split rate.",
    x$id, x$split)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @export
print.SubsampleDesc = function(x, ...) {
  catf("Resample description: %s with %i iterations and %.2f split rate.",
    x$id, x$iters, x$split)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @export
print.RepCVDesc = function(x, ...) {
  catf("Resample description: %s with %i iterations: %i folds and %i reps.",
    x$id, x$iters, x$iters/x$reps, x$reps)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}
