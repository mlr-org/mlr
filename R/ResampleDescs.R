makeResampleDescHoldout = function(iters, split = 2/3) {
  assertNumber(split, lower = 0, upper = 1)
  makeResampleDescInternal("holdout", iters = 1L, split = split)
}

makeResampleDescCV = function(iters = 10L) {
  iters = asCount(iters, positive = TRUE)
  makeResampleDescInternal("cross-validation", iters = iters)
}

makeResampleDescLOO = function() {
  makeResampleDescInternal("LOO", iters = NA_integer_)
}

makeResampleDescSubsample = function(iters = 30L, split = 2/3) {
  iters = asCount(iters, positive = TRUE)
  assertNumber(split, lower = 0, upper = 1)
  makeResampleDescInternal("subsampling", iters = iters, split = split)
}

makeResampleDescBootstrap = function(iters = 30L) {
  iters = asCount(iters, positive = TRUE)
  makeResampleDescInternal("OOB bootstrapping", iters = iters)
}

makeResampleDescRepCV = function(reps = 10L, folds = 10L) {
  reps = asInt(reps, lower = 2L)
  folds = asInt(folds, lower = 2L)
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
