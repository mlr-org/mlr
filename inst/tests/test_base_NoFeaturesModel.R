context("NoFeaturesModel")

test_that("NoFeaturesModel", {
  task = subsetTask(multiclass.task, features=character(0))
	lrn = makeLearner("classif.lda", predict.type="prob")
	m = train(lrn, task)
  expect_is(m$learner.model, "NoFeaturesModel")	
  expect_is(m$learner, "classif.lda")	
	expect_equal(m$learner$predict.type, "prob")	
	p = predict(m, newdata=multiclass.df)
	expect_true(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	res = makeResampleDesc("CV", iter=2)
	rf = resample(lrn, task, res)
	expect_true(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	
	task = subsetTask(regr.task, features=character(0))
  lrn = makeLearner("regr.lm")
  m = train(lrn, task)
	p = predict(m, newdata=regr.df)
	expect_true(all(p$data$response == mean(p$data$response))) 
  rf = resample(lrn, task, res)$pred
	expect_equal(length(unique(rf$data$response)), 2) 
})