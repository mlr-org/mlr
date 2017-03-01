context("FDA_FeatExtraction")

test_that("FDA_FeatExtraction", {
  gp = getTaskData(gunpoint.task, target.extra = TRUE)
  d = extractFDAFeatures(data = gp$data, target = getTaskTargetNames(gunpoint.task), method = "wavelets", args = list( filter = "d4", boundary = "reflection"))
  expect_true(dim(d)[1] == dim(gp$data)[1])
  #expect_true(all(cc == dd))
})


test_that("FDA_MultiFeatExtraction", {
  fuel = getTaskData(fuelSubset.task, target.extra = TRUE)
  tdesc = getTaskDescription(fuelSubset.task)
  d = extractMultiFDAFeatures(data = fuel$data, target = getTaskTargetNames(fuelSubset.task), fd.features = tdesc$fd.features , method = "wavelets", args = list( filter = "d4", boundary = "reflection"))
  expect_true(dim(d)[1] == dim(fuel$data)[1])
  #expect_true(all(cc == dd))
})
