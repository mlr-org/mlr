context("FDA_DTW")

test_that("FDA_DTW", {
  task = subsetTask(fuelsubset.task, features = "UVVIS")
  daf = getTaskData(task, functionals.as = "matrix")
  # df = df$UVVIS
  daf = daf[, "UVVIS"]
  #df = data.frame(as.list(df))
  #dtw.refs = lapply(c(1,129), function(i) as.vector(as.matrix(df[i,])))
  dtw.refs = lapply(c(1,129), function(i) as.vector(daf[i,]))
  fmethods = list("UVVIS" = extractFDADTWKernel(dtw.refs = dtw.refs))
  res = extractFDAFeatures(fuelsubset.task, feat.methods = fmethods)
  # check output data
  df = getTaskData(res$task)
  expect_is(df, "data.frame")
  expect_equal(nrow(df), 129)
})

