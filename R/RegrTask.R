#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, weights, blocking, check.data=TRUE) {
  addClasses(makeSupervisedTask("regr", id, data, target, weights, blocking, NA_character_, check.data),
    "RegrTask")
}
