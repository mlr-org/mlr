context("benchmark_merger")

test_that("benchmark merger", {
  rdesc = makeResampleDesc("CV", iters = 2)
  learners = list(makeLearner("classif.rpart"),
    makeLearner("classif.ctree"),
    makeLearner("classif.randomForest"))
  tasks = list(iris.task, bc.task, sonar.task)
  
  # checks if list of unmerged BenchmarkResults is equal to the merged BenchmarkResults
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
  
  # Forgot to run another learner on both tasks
  bench3 = benchmark(learners[3], tasks[1:2], rdesc)
  merged = mergeBenchmarkResultLearner(bench, bench3)
  checkBenchmarkResult(list(bench, bench3), merged)
  
  # Forgot to run all learners on another task
  bench3 = benchmark(learners[1:2], tasks[[3]], rdesc)
  merged = mergeBenchmarkResultTask(bench, bench3)
  checkBenchmarkResult(list(bench, bench3), merged)
  
  # Change order of tasks and see if merging works
  bench1 = benchmark(learners[[1]], tasks[1:2], rdesc)
  bench2 = benchmark(learners[[2]], tasks[2:1], rdesc)
  merged = mergeBenchmarkResultLearner(bench1, bench2)
  checkBenchmarkResult(list(bench1, bench2), merged)
  
  # Merge both learner and task
  bench1 = benchmark(learners[[1]], tasks[1:2], rdesc)
  # apply two additional lerners on same tasks
  bench2 = benchmark(learners[2:3], tasks[1:2], rdesc)
  # add another task and apply all on it learners
  bench3 = benchmark(learners, tasks[3], rdesc)
  # merge all learners
  result = mergeBenchmarkResultTask(mergeBenchmarkResultLearner(bench1, bench2), bench3)
  checkBenchmarkResult(list(bench1, bench2, bench3), result)
  
  # error message check
  l1t1 = benchmark(learners[[1]], tasks[[1]], rdesc)
  l2t1 = benchmark(learners[[2]], tasks[[1]], rdesc)
  l1t2 = benchmark(learners[[1]], tasks[[2]], rdesc)
  l2t2 = benchmark(learners[[2]], tasks[[2]], rdesc)
  
  expect_error(mergeBenchmarkResultLearner(l2t1, l2t2), "duplicated learner")
  expect_error(mergeBenchmarkResultLearner(l1t1, l1t2), "duplicated learner")
  expect_error(mergeBenchmarkResultTask(l1t1, l2t1), "duplicated task")
  expect_error(mergeBenchmarkResultTask(l1t2, l2t2), "duplicated task")
  
  expect_error(mergeBenchmarkResultLearner(l1t1, l2t2), "based on the same set of tasks")
  expect_error(mergeBenchmarkResultLearner(l1t2, l2t1), "based on the same set of tasks")
  expect_error(mergeBenchmarkResultTask(l1t1, l2t2), "based on the same set of learners")
  expect_error(mergeBenchmarkResultTask(l1t2, l2t1), "based on the same set of learners")
  
  expect_true(inherits(mergeBenchmarkResultLearner(l1t1, l2t1), "BenchmarkResult"))
  expect_true(inherits(mergeBenchmarkResultLearner(l1t2, l2t2), "BenchmarkResult"))
  expect_true(inherits(mergeBenchmarkResultTask(l1t1, l1t2), "BenchmarkResult"))
  expect_true(inherits(mergeBenchmarkResultTask(l2t1, l2t2), "BenchmarkResult"))
  
  # try to merge more than two benchmarkResults
  l3t1 = benchmark(learners[[3]], tasks[[1]], rdesc)
  l1t3 = benchmark(learners[[1]], tasks[[3]], rdesc)
  ml = mergeBenchmarkResultLearner(l1t1, l2t1, l3t1)
  mt = mergeBenchmarkResultTask(l1t1, l1t2, l1t3)
  
  checkBenchmarkResult(list(l1t1, l2t1, l3t1), ml)
  checkBenchmarkResult(list(l1t1, l1t2, l1t3), mt)
})
