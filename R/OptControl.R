makeOptControl = function(same.resampling.instance, ...) {
  setClasses(list(
      same.resampling.instance = same.resampling.instance,
      extra.args = list(...)),
    "OptControl")
}
