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
  status = sample(0:1, N, prob=c(2, 8), replace=TRUE)
  s = Surv(time, status)
  data = cbind(time, status, iris[seq(1, 150, length.out=N), ])
  target = c("time", "status")

  task = makeSurvTask("testtask", data, target = c("time", "status"))
  getTaskData(task, target.extra=TRUE)
  subsetTask(task, 1:3, features="Species")
}
