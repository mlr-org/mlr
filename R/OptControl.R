makeOptControl = function(same.resampling.instance, impute.val = NULL, tune.threshold = FALSE, ...) {
  assertFlag(same.resampling.instance)
  if (!is.null(impute.val))
    assertNumber(impute.val)
  assertFlag(tune.threshold)
  makeS3Obj("OptControl",
    same.resampling.instance = same.resampling.instance,
    impute.val = impute.val,
    tune.threshold = tune.threshold,
    extra.args = list(...)
  )
}
