#' @export
#' @rdname SupervisedTask
makeSurvTask = function(id, data, target, weights, blocking, check.data=TRUE) {
  addClasses(makeSupervisedTask("surv", id, data, target, weights, blocking, NA_character_, check.data),
    "SurvivalTask")
}

if (FALSE) {
  library(survival)
  N = 10
  time = rexp(N, 0.5) + 0.1
  event = sample(0:1, N, prob=c(2, 8), replace=TRUE)
  s = Surv(time, event)
  data = cbind(time, event, iris[seq(1, 150, length.out=N), ])

  task = makeSurvTask("testtask", data, target = c("time", "event"))
}
