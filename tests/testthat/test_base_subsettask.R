context("test select response labels for subsettask")

test_that("test select response labels for subsettask", {
  yeast = getTaskData(yeast.task)
  labels = colnames(yeast)[1:14]
  yeast.task = makeMultilabelTask(id = "multi", data = yeast, target = labels)
  yeast.task
  new_labels = colnames(yeast)[1:4]
  subtask = subsetTask(yeast.task, labels = new_labels)
  subtask$target
  expect_equal(subtask$target, new_labels)
})
