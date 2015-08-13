context("Feating Ensemble Algorithm")

test_that("Feating works",{
  lrn = makeLearner("classif.svm")
  
  # binary prob
  flrn = makeFeatingEnsemble(learner = lrn, attrs = 1:5, random = 5, maximum.level = 1, 
                             minimum.instance = 4, predict.type = "prob")
  m = train(flrn, binaryclass.task)
  pred = predict(m, newdata = binaryclass.df)
  
  #binary response
  flrn = makeFeatingEnsemble(learner = lrn, attrs = 1:5, random = 5, maximum.level = 1, 
                             minimum.instance = 4, predict.type = "response")
  m = train(flrn, binaryclass.task)
  pred = predict(m, newdata = binaryclass.df)
  
  #multi prob
  flrn = makeFeatingEnsemble(learner = lrn, attrs = 1:4, random = 5, maximum.level = 1, 
                             minimum.instance = 4, predict.type = "prob")
  m = train(flrn, multiclass.task)
  pred = predict(m, newdata = multiclass.df)
  
  #multi response
  flrn = makeFeatingEnsemble(learner = lrn, attrs = 1:4, random = 5, maximum.level = 1, 
                             minimum.instance = 4, predict.type = "response")
  m = train(flrn, multiclass.task)
  pred = predict(m, newdata = multiclass.df)
  
  #test constant leaf
  flrn = makeFeatingEnsemble(learner = lrn, attrs = 1:4, random = 5, maximum.level = 1, 
                             minimum.instance = 140, predict.type = "prob")
  m = train(flrn, multiclass.task)
  pred = predict(m, newdata = multiclass.df)
  
  flrn = makeFeatingEnsemble(learner = lrn, attrs = 1:4, random = 5, maximum.level = 1, 
                             minimum.instance = 140, predict.type = "response")
  m = train(flrn, multiclass.task)
  pred = predict(m, newdata = multiclass.df)
})
