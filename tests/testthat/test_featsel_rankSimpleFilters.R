context("filters")

test_that("base filters of ensemble filters are ranked correctly", {
  requirePackagesOrSkip("Hmisc", default.method = "load")

  filters.ranked = rankBaseFilters(task.filters.rank,
    method = c("univariate.model.score", "variance"),
    nselect = 9, more.args = list())

  # split into groups to check the ordering of vars "value" and "rank" for each
  data.split = split(filters.ranked, filters.ranked$filter)

  foo = lapply(data.split, function(x) {
    expect_false(is.unsorted(x[["value"]]))
    expect_false(is.unsorted(x[["rank"]]))
  })

  # check that the highest value has also the highest rank
  expect_true(which.max(data.split[[1]]$value) == which.max(data.split[[1]]$rank))
  expect_true(which.max(data.split[[2]]$value) == which.max(data.split[[2]]$rank))
})
