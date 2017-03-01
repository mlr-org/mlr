context("FDA_FeatExtraction")

test_that("FDA_FeatExtraction", {
  gp = getTaskData(gunpoint.task, target.extra = TRUE)
  extractFDAFeatures(data = gp$data, target = getTaskTargetNames(gunpoint.task), method = "wavelets", list( filter = "d4", boundary = "reflection"))
  #expect_true(all(bb == aa))
  #expect_true(all(cc == dd))
})



test_that("FDA_MultiFeatExtraction", {
  #gp = getTaskData(fuelSubset.task, target.extra = TRUE)
  #extractMultiFDAFeatures(data = gp$data, target = getTaskTargetNames(gunpoint.task), "wavelets", args = list( filter = "d4", boundary = "reflection"))
  #expect_true(all(bb == aa))
  #expect_true(all(cc == dd))
})
