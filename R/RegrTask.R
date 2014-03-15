#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, weights, blocking, fixup.data = "warn", check.data = TRUE) {
  addClasses(makeSupervisedTask("regr", id, data, target, weights, blocking, NA_character_, fixup.data, check.data),
    "RegrTask")
}
