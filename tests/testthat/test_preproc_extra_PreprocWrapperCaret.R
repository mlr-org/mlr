context("PreprocWrapperCaret")

test_that("basic PreprocWrapperCaret works", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makePreprocWrapperCaret(lrn1, method = c("center"))
  m = train(lrn2, multiclass.task)  
  p = predict(m, multiclass.task)
  perf = performance(p, mmce)
  expect_true(perf < 0.1)
})
