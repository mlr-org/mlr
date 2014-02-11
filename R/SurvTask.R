#' @export
#' @rdname SupervisedTask
makeSurvTask = function(id, data, target, weights, blocking, check.data=TRUE) {
  addClasses(makeSupervisedTask("surv", id, data, target, weights, blocking, NA_character_, check.data),
    "SurvivalTask")
}

if (FALSE) {
  library(survival)
  N = 150
  train = sample(N, 2/3 * N)
  test = setdiff(seq_len(N), train)

  time = rexp(N, 0.5) + 0.1
  status = sample(0:1, N, prob=c(2, 8), replace=TRUE)
  s = Surv(time, status)
  data = cbind(time, status, iris)
  target = c("time", "status")

  task = makeSurvTask("testtask", data, target = target)
  task2 = subsetTask(task, train)

  lrn = makeLearner("surv.coxph")
  trained = train(lrn, task2)
  pred = predict(trained, newdata=getTaskData(task, test))
  performance(pred, cindex)



  # data = data.frame(time=1:20, event=rep(1, 20), x=1:20+runif(20))
  #
  #
  # s = with(data, Surv(time, event))
  # mod = coxph(s ~ x, data=data)
  # lp = predict(mod, data)
  # rcorr.cens(lp, s)
  #
  # task = makeSurvTask(data=data, target=c("time", "event"))
  # lrn = makeLearner("surv.coxph")
  # trained = train(lrn, task)
  # pred = predict(trained, newdata=getTaskData(task, test))
  # performance(pred, cindex)

  # task = makeClassifTask("iris", iris, target="Species")
  # predict(train(makeLearner("classif.rpart"), task), task)
}
