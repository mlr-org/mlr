lrn.regr = makeLearner("regr.ksvm")
fit.regr = train(lrn.regr, bh.task)

getTaskFeatureNames(bh.task)
pd.regr = generatePartialPredictionData(fit.regr, bh.task, c("crim", "age"), interaction = TRUE)

#p = plotPartialPrediction(pd.regr, three.d)
#print(p)
#plotPartialPredictionGGVIS(pd.regr)


plotPartialPredictionPlotly(pd.regr)  function(){
  
  
  
  
  
  
  
}