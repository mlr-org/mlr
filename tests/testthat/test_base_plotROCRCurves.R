context("plotROCRCurves")

test_that("plotROCRCurves", {
              lrn1 = makeLearner("classif.logreg", predict.type = "prob")
              lrn2 = makeLearner("classif.rpart", predict.type = "prob")
              b = benchmark(list(lrn1, lrn2), pid.task)
              plotROCRCurves(b)
              plotROCRCurves(b, interactive = TRUE)

              res = crossval(lrn1, pid.task)
              plotROCRCurves(res)

              fit = train(lrn1, pid.task)
              pred = predict(fit, pid.task)
              plotROCRCurves(pred)
          })
