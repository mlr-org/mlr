context("benchmark merger")

test_that("benchmark merger", {
  rdesc = makeResampleDesc("CV", iters = 2)
  learners = list(makeLearner("classif.rpart"),
    makeLearner("classif.ctree"),
    makeLearner("classif.randomForest"))
  tasks = list(iris.task, bc.task, sonar.task)
  
  # checks if list of single BenchmarkResults is equal to the merged BenchmarkResults
  checkBenchmarkResult = function(list, merged) {
    rbinded = do.call("rbind", lapply(list, as.data.frame))
    res = merge(rbinded, merged, by = c("task.id", "learner.id", "iter"), all = TRUE)
    expect_true(nrow(res) == nrow(rbinded))
    expect_true(all(c("mmce.x", "mmce.y")%in%colnames(res)))
    expect_true(identical(res$mmce.x, res$mmce.y))
  }
  
  # Change order of leraners and see if merging works
  bench1 = benchmark(learners[1:2], tasks[[1]], rdesc)
  bench2 = benchmark(learners[2:1], tasks[[2]], rdesc)
  bench = mergeBenchmarkResultTask(bench1, bench2)
  checkBenchmarkResult(list(bench1, bench2), bench)
  
  # Forgot to run a learner on both tasks
  bench3 = benchmark(learners[3], tasks[1:2], rdesc)
  merged = mergeBenchmarkResultLearner(bench, bench3)
  checkBenchmarkResult(list(bench, bench3), merged)
  
  # Forgot to run all learners on another task
  bench3 = benchmark(learners[1:2], tasks[[3]], rdesc)
  merged = mergeBenchmarkResultTask(bench, bench3)
  checkBenchmarkResult(list(bench, bench3), merged)
  
  # Change order of tasks and see if merging works
  bench1lrn = benchmark(learners[[1]], tasks[1:2], rdesc)
  bench2lrn = benchmark(learners[[2]], tasks[2:1], rdesc)
  benchLearner = mergeBenchmarkResultLearner(bench1lrn, bench2lrn)
  checkBenchmarkResult(list(bench1lrn, bench2lrn), benchLearner)
  
  # Merge both
  bench1 = benchmark(learners[[1]], tasks[1:2], rdesc)
  # apply two additional lerners on same task
  bench2 = benchmark(learners[2:3], tasks[1:2], rdesc)
  # add another task and apply all on it learners
  bench3 = benchmark(learners, tasks[3], rdesc)
  # merge all learners
  result = mergeBenchmarkResultTask(mergeBenchmarkResultLearner(bench1, bench2), bench3)
  checkBenchmarkResult(list(bench1, bench2, bench3), result)
})