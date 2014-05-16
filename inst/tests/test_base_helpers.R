context("helpers")

if (interactive()) {

test_that("makeOptPathDFFromMeasures", {
  measures = list(mae, auc)
  par.set = makeParamSet(
    makeIntegerParam("a"),
    makeIntegerParam("b")
  )
  opt.path = makeOptPathDFFromMeasures(par.set, measures)
  addOptPathEl(opt.path, x=list(1L, 1L), y=c(0.5, 0.5))
  res = getOptPathEl(opt.path, 1L)
  supposed.names = sapply(measures, function(m) paste(m$id, m$aggr$id, sep="."))
  expect_equal(names(res$y), supposed.names)
})

}
