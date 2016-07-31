context("stack_aggregatePredictions")

# classif
pts = c("prob", "response")
tasks = list(binaryclass.task, multiclass.task)

# tsk = subsetTask(iris.task, c(90:150, 1:89)); pt = "prob"; sm.pt = "prob"
for (tsk in tasks) {
  for (pt in pts) {
    for (sm.pt in pts) {
      #messagef("This is %s, %s, %s \n", tsk$task.desc$id, pt, sm.pt)
      lrn0 = makeLearner("classif.rpart", predict.type = pt)
      lrn1 = makeLearner("classif.kknn", predict.type = pt)
      lrn2 = makeLearner("classif.kknn", k = 10, predict.type = pt)
  
      tr0 = train(lrn0, tsk)
      tr1 = train(lrn1, tsk)
      tr2 = train(lrn2, tsk)
      pr0 = predict(tr0, tsk)
      pr1 = predict(tr1, tsk)
      pr2 = predict(tr2, tsk)
      
      pred.list = list(pr0, pr1, pr2)
      like_pr0 = aggregatePredictions(pred.list[1], sm.pt = sm.pt, pL = FALSE)
      expect_true(identical(pr0, like_pr0))
      p = aggregatePredictions(pred.list, sm.pt = sm.pt, pL = FALSE)
      p #%>% print
      perf = performance(p) #%>% print
      expect_that(perf < 1/2, is_true())
    }
  }
}

# regr
tasks = list(regr.task, regr.small.task, regr.num.task)
lrn0 = makeLearner("regr.rpart")
lrn1 = makeLearner("regr.kknn")
lrn2 = makeLearner("regr.kknn", k = 10, predict.type = pt)

for (tsk in tasks) {
  #messagef("This is %s\n", tsk$task.desc$id)
  tr0 = train(lrn0, tsk)
  tr1 = train(lrn1, tsk)
  tr2 = train(lrn2, tsk)
  pr0 = predict(tr0, tsk)
  pr1 = predict(tr1, tsk)
  pr2 = predict(tr2, tsk)
  
  pred.list = list(pr0, pr1, pr2)
  p = aggregatePredictions(pred.list, sm.pt = NULL, pL = FALSE)
  p #%>% print
  perf = performance(p) #%>% print
  expect_that(perf > 0.1, is_true())
}