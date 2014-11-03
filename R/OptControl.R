makeOptControl = function(same.resampling.instance, impute.val = NULL, tune.threshold = FALSE, ...) {
  makeS3Obj("OptControl",
    same.resampling.instance = same.resampling.instance,
    impute.val = impute.val,
    tune.threshold = tune.threshold,
    extra.args = list(...)
  )
}
