context("FDA_subsetTask")
test_that("FDA_subsetTask", {
  st = subsetTask(fuelSubset.task, features = 1:200)
  expect_equal(length(st$task.desc$fd.features$NIR), 66L)
  expect_equal(length(st$task.desc$fd.features$UVVIS), 134L)
  
  st.gp = subsetTask(gunpoint.task, features = 1:10)
  expect_equal(length(st.gp$task.desc$fd.features$fd1), 10L)
})