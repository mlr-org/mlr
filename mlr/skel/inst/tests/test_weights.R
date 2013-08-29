context("weights")

test_that("weights", {
  lrn = makeLearner("regr.lm")
  ws = 1:nrow(regr.df)
	rt = makeRegrTask(target=regr.target, data=regr.df)
	m = train(lrn, task=rt, weights=ws)
	p = predict(m, task=rt, subset=30:100)
	df = as.data.frame(p)
	cns = colnames(df)
	expect_equal(cns, c("id", "truth", "response"))

	# glm bug, we need do.call
	m2 = do.call(lm, list(regr.formula, data=regr.df, weights=ws))
	p2 = predict(m2, newdata=regr.df[30:100,])
	expect_equal(p2, p$data$response, check.attributes=FALSE)
	
	expect_error(train(lrn, rger.task, weights=1:2))
})
