makeOptControl = function(same.resampling.instance, ...) {
  structure(list(
    same.resampling.instance = same.resampling.instance,
		extra.args = list(...)
  ), class="OptControl")
}
