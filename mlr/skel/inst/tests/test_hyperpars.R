context("hyperpars")

test_that("hyperpars", {
	lrn = makeLearner("classif.rpart", minsplit=10)
	expect_equal(getHyperPars(lrn), list(xval=0, minsplit=10)) 
	
	m = train(lrn, task=multiclass.task)
  expect_true(!inherits(m, "FailureModel"))
	expect_equal(getHyperPars(m$learner), list(xval=0, minsplit=10)) 
	
  # check warnings
  configureMlr(on.par.without.desc="warn")  
  expect_warning(makeLearner("classif.rpart", foo=1), "Setting parameter foo without")  
  #FIXME can only check this when testthat is updated
  #configureMlr(on.par.without.desc="quiet")
  #expect_warning(makeLearner("classif.rpart", foo=1), FALSE)  
  configureMlr()
})

