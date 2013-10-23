context("tune")

# fixme: test tuning of chain, maybe in mlrChains

test_that("tune", {
  library(e1071)
  cp = c(0.05, 0.9)
  minsplit = 1:3 
  ps1 = makeParamSet(
    makeDiscreteParam("cp", values=cp), 
    makeDiscreteParam("minsplit", values=minsplit)
  )
	ctrl = makeTuneControlGrid()
	folds = 3
	
	tr = tune.rpart(formula=multiclass.formula, data=multiclass.df, cp=cp, minsplit=minsplit,
			tunecontrol = tune.control(sampling = "cross", cross = folds))  
	lrn = makeLearner("classif.rpart")
	cv.instance = e1071CVToMlrCV(tr)
	m1 = setAggregation(mmce, test.mean)
  m2 = setAggregation(mmce, test.sd)
	tr2 = tuneParams(lrn, multiclass.task, cv.instance, par.set=ps1, control=ctrl, measures=list(m1, m2))
  pp = as.data.frame(tr2$opt.path)  
	# todo test scale with tune.e1071 and scaled grid!	
	for(i in 1:nrow(tr$performances)) {
		cp = tr$performances[i,"cp"]
		ms = tr$performances[i,"minsplit"]
		j = which(pp$cp == cp & pp$minsplit == ms )
		expect_equal(tr$performances[i,"error"], pp[j,"mmce.test.mean"])    
		expect_equal(tr$performances[i,"dispersion"], pp[j,"mmce.test.sd"])    
	}
  # test printing
  capture.output(print(tr2))
  
	# check multiple measures
	ms = c("acc", "mmce", "timefit") 
	tr2 = tuneParams(lrn, multiclass.task, cv.instance, par.set=ps1, control=ctrl)
  
  expect_error(tuneParams(lrn, multiclass.task, cv.instance, par.set=makeParamSet(), control=ctrl))
})


