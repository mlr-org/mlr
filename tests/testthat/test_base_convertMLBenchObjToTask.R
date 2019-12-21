context("convertMLBenchObjToTask")

test_that("convertMLbenchObjToTask", {

  requirePackagesOrSkip("mlbench")
  # get all mlbench.* functions, 1spiral does not work
  fs = ls("package:mlbench", pattern = "mlbench")
  n = 77L
  for (f in setdiff(fs, "mlbench.1spiral")) {
    task = convertMLBenchObjToTask(f, n = n)
    expect_is(task, "Task")
    # for some, n is not properly respected in mlbench
    if (f %nin% c("mlbench.corners", "mlbench.hypercube", "mlbench.simplex")) {
      expect_equal(getTaskSize(task), n)
    }
  }

  # get all mlbench datasets, HouseVotes84 and Ozone have NAs in target col
  ds = data(package = "mlbench")
  ds = ds$results[, "Item"]
  for (d in setdiff(ds, c("HouseVotes84", "Ozone"))) {
    task = convertMLBenchObjToTask(d, n = n)
    expect_is(task, "Task")
    expect_equal(getTaskId(task), d)
  }
})
