context("plotROCRCurves")

test_that("plotROCRCurves", {
    lrn1 = makeLearner("classif.lda", predict.type = "prob")
    lrn2 = makeLearner("classif.rpart", predict.type = "prob")
    mod1 = train(lrn1, sonar.task)
    mod2 = train(lrn2, sonar.task)
    pred1 = predict(mod1, task = sonar.task)
    pred2 = predict(mod2, task = sonar.task)
    b = benchmark(list(lrn1, lrn2), sonar.task)

    plotROCRCurves(pred1)
    plotROCRCurves(pred2)
    plotROCRCurves(list("lda" = pred1, "rpart" = pred2))
    plotROCRCurves(b)
})
