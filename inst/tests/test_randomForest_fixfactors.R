context("random_forest_fix_factors")

test_that("fix factors work", {
	data(iris)
	n = nrow(iris)
  data = iris
	train = sample(1:n, floor(n * 0.9))
	test = setdiff(1:n, train)

  task = makeRegrTask(data=data[train, ], target="Sepal.Length")
  learner = makeLearner("regr.randomForest", fix.factors=TRUE)
  model = train(learner, task)
  newdata = data[head(test, 1L), ]
  newdata$Species = droplevels(newdata$Species)
  expect_is(predict(model, newdata=newdata), "Prediction")

  data$x = factor(sample(letters[1:3], n, replace=TRUE))
  task = makeClassifTask(data=data[train, ], target="Species")
  learner = makeLearner("classif.randomForest", fix.factors=TRUE)
  model = train(learner, task)
  newdata = data[head(test, 1L), ]
  newdata$Species = droplevels(newdata$Species)
  expect_is(predict(model, newdata=newdata), "Prediction")
})
