makeOptControl = function(same.resampling.instance, impute.val = Inf, ...) {
  makeS3Obj("OptControl",
    same.resampling.instance = same.resampling.instance,
    impute.val = impute.val,
    extra.args = list(...)
  )
}
