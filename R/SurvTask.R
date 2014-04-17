#' @export
#' @rdname SupervisedTask
makeSurvTask = function(id, data, target, weights, blocking, fixup.data = "warn", check.data = TRUE) {
  addClasses(makeSupervisedTask("surv", id, data, target, weights, blocking, NA_character_, fixup.data, check.data),
    "SurvTask")
}
