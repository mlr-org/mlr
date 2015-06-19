context("RankMatrix")

test_that("RankMatrix", {
  
  #Get Data
  lrns = list(makeLearner("classif.randomForest"), makeLearner("classif.nnet"),
              makeLearner("classif.rpart"), makeLearner("classif.svm"))
  tasks = list(iris.task, sonar.task, pid.task)
  rdesc = makeResampleDesc("CV", iters = 5)
  meas = list(acc, mmce, ber, featperc)
  res = benchmark(lrns, tasks, rdesc, meas)
  nTasks = length(getBMRTaskIds(res))
  nLrns = length(getBMRLearnerIds(res))
  
  
  # Create vector with learner.ids and task.ids
  lrn.id_1 = c("classif.svm","classif.randomForest", "classif.nnet",
               "classif.rpart")
  lrn.id_2 = getBMRLearnerIds(res)[c(1L,4L,3L,2L)]
  tsk.id_1 = getBMRTaskIds(res)[c(1L,3L,2L)]
  
  
  # Test ordering Lrns
  # Test Class
  r_1 = orderBMRLrns(res, order.Lrns = lrn.id_1)
  expect_is(r_1,"data.frame")
  r_2 = orderBMRLrns(res, order.Lrns = lrn.id_2)
  expect_is(r_2, "data.frame")
  r_3 = orderBMRLrns(res, order.Lrns = c(4L, 3L, 2L, 1L))
  expect_is(r_3, "data.frame")
  # Test constant dimensions
  expect_equal(dim(r_1), dim(as.data.frame(res)))
  expect_equal(dim(r_2), dim(as.data.frame(res)))
  expect_equal(dim(r_3), dim(as.data.frame(res)))

    
  # Test ordering Tsks
  r_1 = orderBMRTasks(res, order.Tsks = c(3L,2L,1L))
  expect_is(r_2, "data.frame")
  r_2 = orderBMRTasks(res, order.Tsks = tsk.id_1)
  expect_is(r_2, "data.frame")
  expect_equal(dim(r_1), dim(as.data.frame(res)))
  expect_equal(dim(r_2), dim(as.data.frame(res)))
  
})
