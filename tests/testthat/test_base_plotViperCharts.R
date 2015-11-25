context("plotViperCharts")


# FIXME: vipercharts URL seems offline, dont know why, see #571

# test_that("plotViperCharts", {
#   lrn1 = makeLearner("classif.rpart", predict.type = "prob")
#   lrn2 = makeLearner("classif.lda", predict.type = "prob")
#   lrns = list(lrn1, lrn2)
#   m = train(lrn1, binaryclass.task)
#   p = predict(m, binaryclass.task)
#   plotViperCharts(p, browse = FALSE)

#   br = benchmark(lrn2, binaryclass.task, resampling = makeResampleDesc("Holdout"))
#   plotViperCharts(p, browse = FALSE)

#   rs = lapply(lrns, holdout, task = binaryclass.task)
#   names(rs) = c("a", "b")
#   plotViperCharts(rs, browse = FALSE)
# })


