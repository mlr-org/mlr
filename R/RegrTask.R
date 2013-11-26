#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, weight, blocking, check.data=TRUE) {
  task = makeSupervisedTask("regr", id, data, target, weight, blocking, NA_character_, check.data)
  class(task) = c("RegrTask", class(task))
  return(task)
}
