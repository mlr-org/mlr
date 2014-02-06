makeResampleDescHoldout = function(iters, split=2/3) {
  checkArg(split, "numeric", len=1L, na.ok=FALSE, lower=0, upper=1)
  makeResampleDescInternal("holdout", iters=1L, split=split)
}

makeResampleDescCV = function(iters=10L) {
  iters = convertInteger(iters)
  checkArg(iters, "integer", len=1L, na.ok=FALSE, lower=1L)
  makeResampleDescInternal("cross-validation", iters=iters)
}

makeResampleDescLOO = function() {
  makeResampleDescInternal("LOO", iters=NA_integer_)
}

makeResampleDescSubsample = function(iters=30L, split=2/3) {
  iters = convertInteger(iters)
  checkArg(iters, "integer", len=1L, na.ok=FALSE, lower=1L)
  checkArg(split, "numeric", len=1L, na.ok=FALSE, lower=0, upper=1)
  makeResampleDescInternal("subsampling", iters=iters, split=split)
}

makeResampleDescBootstrap = function(iters=30L) {
  iters = convertInteger(iters)
  checkArg(iters, "integer", len=1L, na.ok=FALSE, lower=1L)
  makeResampleDescInternal("OOB bootstrapping", iters=iters)
}

makeResampleDescRepCV = function(reps=10L, folds=10L) {
  reps = convertInteger(reps)
  checkArg(reps, "integer", len=1L, na.ok=FALSE, lower=2L)
  folds = convertInteger(folds)
  checkArg(folds, "integer", len=1L, na.ok=FALSE, lower=2L)
  makeResampleDescInternal("repeated cross-validation", iters=folds*reps, folds=folds, reps=reps)
}

#' @S3method print HoldoutDesc
print.HoldoutDesc = function(x, ...) {
  catf("Resample description: %s with %.2f split rate.",
    x$id, x$split)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @S3method print SubsampleDesc
print.SubsampleDesc = function(x, ...) {
  catf("Resample description: %s with %i iterations and %.2f split rate.",
    x$id, x$iters, x$split)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}

#' @S3method print RepCVDesc
print.RepCVDesc = function(x, ...) {
  catf("Resample description: %s with %i iterations: %i folds and %i reps.",
    x$id, x$iters, x$iters/x$reps, x$reps)
  catf("Predict: %s", x$predict)
  catf("Stratification: %s", x$stratify)
}
