Training a Learner
==================

Training a learner just means fitting a model to a given data set.
In the **mlr** package this can be done by calling the function [train](http://berndbischl.github.io/mlr/man/train.html) that 
provides a unified interface to all integrated [learners](http://berndbischl.github.io/mlr/man/learners.html) passing a learner 
and a [LearnTask](http://berndbischl.github.io/mlr/man/makeLearner.html).


Quick start
-----------

### Classification example

We start with a basic example and train a Linear Discriminant Analysis on the ``iris`` data set.


```splus
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
mod = train(lrn, task)
mod
```

```
## Model for id=classif.lda class=classif.lda
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

task = makeRegrTask(data = BostonHousing, target = "medv")
lrn = makeLearner("regr.lm")
mod = train(lrn, task)
mod
```

```
## Model for id=regr.lm class=regr.lm
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
can subsequently be used to perform a [prediction](http://berndbischl.github.io/mlr/man/predict.WrappedModel.html) for new observations.
