context("mergeBenchmarkResults")

test_that("mergeBenchmarkResults", {
  learners = list(makeLearner("classif.rpart"),
    makeLearner("classif.ctree"),
    makeLearner("classif.randomForest", ntree = 2))
  tasks = list(iris.task, bc.task, sonar.task)

  # checks if list of unmerged BenchmarkResults is equal to the merged BenchmarkResults
  checkBenchmarkResults = function(list, merged) {
    expect_is(merged, "BenchmarkResult")
    rbinded = do.call("rbind", lapply(list, as.data.frame))
    res = merge(rbinded, merged, by = c("task.id", "learner.id", "iter"), all = TRUE)
    expect_true(nrow(res) == nrow(rbinded))
    expect_true(all(c("mmce.x", "mmce.y") %in% colnames(res)))
    expect_true(identical(res$mmce.x, res$mmce.y))
  }

  # Change order of leaners and see if merging works
  bench1 = benchmark(learners[1:2], tasks[[1L]], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  bench2 = benchmark(learners[2:1], tasks[[2L]], cv2, measures = getDefaultMeasure(tasks[[2L]]))
  bench = mergeBenchmarkResults(list(bench1, bench2))
  checkBenchmarkResults(list(bench1, bench2), bench)

  # Forgot to run another learner on both tasks
  bench3 = benchmark(learners[3L], tasks[1:2], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  merged = mergeBenchmarkResults(list(bench, bench3))
  checkBenchmarkResults(list(bench, bench3), merged)

  # Forgot to run all learners on another task
  bench3 = benchmark(learners[1:2], tasks[[3L]], cv2, measures = getDefaultMeasure(tasks[[3L]]))
  merged = mergeBenchmarkResults(list(bench, bench3))
  checkBenchmarkResults(list(bench, bench3), merged)

  # Change order of tasks and see if merging works
  bench1 = benchmark(learners[[1L]], tasks[1:2], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  bench2 = benchmark(learners[[2L]], tasks[2:1], cv2, measures = getDefaultMeasure(tasks[[2L]]))
  merged = mergeBenchmarkResults(list(bench1, bench2))
  checkBenchmarkResults(list(bench1, bench2), merged)

  # Merge both learner and task
  bench1 = benchmark(learners[[1L]], tasks[1:2], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  # apply two additional lerners on same tasks
  bench2 = benchmark(learners[2:3], tasks[1:2], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  # add another task and apply all on it learners
  bench3 = benchmark(learners, tasks[3L], cv2, measures = getDefaultMeasure(tasks[[3L]]))
  # merge all learners
  result = mergeBenchmarkResults(list(bench1, bench2, bench3))
  checkBenchmarkResults(list(bench1, bench2, bench3), result)

  # error message check
  l1t1 = benchmark(learners[[1L]], tasks[[1L]], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  l2t1 = benchmark(learners[[2L]], tasks[[1L]], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  l2t2 = benchmark(learners[[2L]], tasks[[2L]], cv2, measures = getDefaultMeasure(tasks[[2L]]))
  expect_error(mergeBenchmarkResults(list(l1t1, l2t1, l2t1)), "multiple times")
  expect_error(mergeBenchmarkResults(list(l1t1, l2t2)), "are missing")
  # FIXME: do we want to merge BMR with different measures?
  # l1t1.acc = benchmark(learners[[1L]], tasks[[1L]], cv2, measures = acc)
  # expect_error(mergeBenchmarkResults(l2t1, l1t1.acc), "same measures")

  # check measure order
  bench1 = benchmark(learners[1:2], tasks[[1L]], cv2, measures = list(acc, mmce))
  bench2 = benchmark(learners[2:1], tasks[[2L]], cv2, measures = list(mmce, acc))
  result = mergeBenchmarkResults(list(bench1, bench2))
  checkBenchmarkResults(list(bench1, bench2), result)

  # check if recomputing of missing meausures works
  bench1 = benchmark(learners[1:2], tasks[[1L]], cv2, measures = mmce)
  bench2 = benchmark(learners[2:1], tasks[[2L]], cv2, measures = acc)
  result = mergeBenchmarkResults(list(bench1, bench2))
  result = as.data.frame(result)
  expect_true(all(c("acc", "mmce") %in% colnames(result)))
  expect_true(!any(is.na(c(result$add, result$mmce))))

  # check inequal task descriptions
  regr.task = bh.task
  bench1 = benchmark(learners, tasks[[1L]], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  bench2 = benchmark("regr.rpart", regr.task, cv2, measures = getDefaultMeasure(regr.task))
  expect_error(mergeBenchmarkResults(list(bench1, bench2)), "Different task types")

  # check if different resamplings for the same task are allowed
  bench1 = benchmark(learners[[1L]], tasks[1:2], cv2, measures = getDefaultMeasure(tasks[[1L]]))
  bench2 = benchmark(learners[[2L]], tasks[[2L]], cv2, measures = getDefaultMeasure(tasks[[2L]]))
  bench3 = benchmark(learners[[2L]], tasks[[1L]], cv3, measures = getDefaultMeasure(tasks[[1L]]))
  expect_error(mergeBenchmarkResults(list(bench1, bench2, bench3)), "Different resample description")
})
