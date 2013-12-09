Training a Learner
==================

Training a learner just means fitting a model to a given data set.
In the **mlr** package this can be done by calling the function [train](http://berndbischl.github.io/mlr/man/train.html) that 
provides a unified interface to all integrated [learners](http://berndbischl.github.io/mlr/man/learners.html) passing a learner 
and a [LearnTask](http://berndbischl.github.io/mlr/man/makeLearner.html).


Quick start
-----------

### Classification example

We start with a basic example and train a Linear Dicriminant Analysis on the ``iris`` data set.


```splus
library("mlr")
task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.lda")
mod <- train(lrn, task)
mod
```

```
## Learner model for id=classif.lda class=classif.lda
## Trained on obs: 150
## Used features: 4
## Hyperparameters:
```


### Regression example

We fit a simple linear regression model to the ``BostonHousing`` data set.


```splus
library("mlr")
library("mlbench")
data(BostonHousing)

task <- makeRegrTask(data = BostonHousing, target = "medv")
lrn <- makeLearner("regr.lm")
mod <- train(lrn, task)
mod
```

```
## Learner model for id=regr.lm class=regr.lm
## Trained on obs: 506
## Used features: 13
## Hyperparameters:
```


Further information
-------------------

As already mentioned, we can train a learner simply by
calling the function [train](http://berndbischl.github.io/mlr/man/train.html), passing it a [Learner](http://berndbischl.github.io/mlr/man/makeLearner.html) and a [task](http://berndbischl.github.io/mlr/man/SupervisedTask.html). 
In the examples above, the classification or regression method was specified 
by special [Learner](http://berndbischl.github.io/mlr/man/makeLearner.html) objects. This allows for a maximum of flexibility, for example it permits setting the hyperparameters of the learner before training. 

Optionally, only a subset of the data, specified by an index set, can be used to 
train the learner. This set is passed using the ``subset`` argument of [train](http://berndbischl.github.io/mlr/man/train.html).

The return value is always an object of class [WrappedModel](http://berndbischl.github.io/mlr/man/makeWrappedModel.html), which wraps the
particular model of the used R classification or regression method. It
can subsequently be used to perform a [prediction](http://berndbischl.github.io/mlr/man/predict.WrappedModel.html) for new
observations.


### Classification example

Let's train a decision tree on the ``iris`` data set.


```splus
library("mlr")

# First, create the classification task and the learner object
task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.rpart")

# Now, train the model with all the instances provided by the iris data.
mod <- train(lrn, task)

# You can print some basic information of the model to the console.
mod
```

```
## Learner model for id=classif.rpart class=classif.rpart
## Trained on obs: 150
## Used features: 4
## Hyperparameters: xval=0
```


Now, a subset of the data (every second observation) is used for training.


```splus
mod <- train(lrn, task, subset = seq(from = 1, to = nrow(iris), by = 2))
mod
```

```
## Learner model for id=classif.rpart class=classif.rpart
## Trained on obs: 75
## Used features: 4
## Hyperparameters: xval=0
```

  
It's still simple to access the wrapped rpart model, but in most cases there won't be a need to do so.


```splus
mod$learner.model
```

```
## n= 75 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 75 50 setosa (0.3333 0.3333 0.3333)  
##   2) Petal.Length< 2.45 25  0 setosa (1.0000 0.0000 0.0000) *
##   3) Petal.Length>=2.45 50 25 versicolor (0.0000 0.5000 0.5000)  
##     6) Petal.Width< 1.65 25  1 versicolor (0.0000 0.9600 0.0400) *
##     7) Petal.Width>=1.65 25  1 virginica (0.0000 0.0400 0.9600) *
```



### Regression example

As a regression example we use the ``BostonHousing`` data set. Let's train a Gradient Boosting Machine and we begin with the whole data set.


```splus
# Create the regression task.
library("mlbench")
data(BostonHousing)

task <- makeRegrTask(data = BostonHousing, target = "medv")
lrn <- makeLearner("regr.gbm", n.trees = 500, distribution = "laplace", interaction.depth = 3)

mod <- train(lrn, task)
mod
```

```
## Learner model for id=regr.gbm class=regr.gbm
## Trained on obs: 506
## Used features: 13
## Hyperparameters: distribution=laplace,n.trees=500,interaction.depth=3
```


Now, we use a subset of the data for training (every second observation).


```splus
mod <- train(lrn, task, subset = seq(1, nrow(BostonHousing), 2))
mod
```

```
## Learner model for id=regr.gbm class=regr.gbm
## Trained on obs: 253
## Used features: 13
## Hyperparameters: distribution=laplace,n.trees=500,interaction.depth=3
```



