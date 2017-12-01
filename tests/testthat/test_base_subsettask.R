context("test select response labels for subsettask")

test_that("test select response labels for subsettask", {
  yeast = getTaskData(yeast.task)
  labels = colnames(yeast)[1:14]
  yeast.task = makeMultilabelTask(id = "multi", data = yeast, target = labels)
  new_labels = colnames(yeast)[1:4]
  subtask = subsetTask(yeast.task, labels = new_labels)
  expect_equal(subtask$task.desc$target, new_labels)
})
