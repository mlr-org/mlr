Training a Learner
==================

Training a learner just means fitting a model to a given data set.
In the **mlr** package this can be done by calling the function [train](http://berndbischl.github.io/mlr/train.html) that 
provides a unified interface to all integrated [learners](http://berndbischl.github.io/mlr/learners.html) passing a learner 
and a [LearnTask](http://berndbischl.github.io/mlr/makeLearner.html).


Quick start
-----------

### Classification example

We start with a basic example and train a Linear Dicriminant Analysis on the ``iris`` data set.


```r
library("mlr")
task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.lda")
mod <- train(lrn, task)
```

```
## Error: cannot coerce class "c("classif.lda", "RLearnerClassif",
## "RLearner", "Learner")" to a data.frame
```

```r
mod
```

```
## Error: Objekt 'mod' nicht gefunden
```


### Regression example

We fit a simple linear regression model to the ``BostonHousing`` data set.


```r
library("mlr")
library("mlbench")
data(BostonHousing)

task <- makeRegrTask(data = BostonHousing, target = "medv")
lrn <- makeLearner("regr.lm")
mod <- train(lrn, task)
```

```
## Error: cannot coerce class "c("regr.lm", "RLearnerRegr", "RLearner",
## "Learner")" to a data.frame
```

```r
mod
```

```
## Error: Objekt 'mod' nicht gefunden
```


Further information
-------------------

As already mentioned, we can train a learner simply by
calling the function [train](http://berndbischl.github.io/mlr/train.html), passing it a [Learner](http://berndbischl.github.io/mlr/makeLearner.html) and a [task](http://berndbischl.github.io/mlr/SupervisedTask.html). 
In the examples above, the classification or regression method was specified 
by special [Learner](http://berndbischl.github.io/mlr/makeLearner.html) objects. This allows for a maximum of flexibility, for example it permits setting the hyperparameters of the learner before training. 

Optionally, only a subset of the data, specified by an index set, can be used to 
train the learner. This set is passed using the ``subset`` argument of [train](http://berndbischl.github.io/mlr/train.html).

The return value is always an object of class [WrappedModel](http://berndbischl.github.io/mlr/makeWrappedModel.html), which wraps the
particular model of the used R classification or regression method. It
can subsequently be used to perform a [prediction](http://berndbischl.github.io/mlr/predict.WrappedModel.html) for new
observations.


### Classification example

Let's train a decision tree on the ``iris`` data set.


```r
library("mlr")

# First, create the classification task and the learner object
task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.rpart")

# Now, train the model with all the instances provided by the iris data.
mod <- train(lrn, task)
```

```
## Error: cannot coerce class "c("classif.rpart", "RLearnerClassif",
## "RLearner", "Learner")" to a data.frame
```

```r

# You can print some basic information of the model to the console.
mod
```

```
## Error: Objekt 'mod' nicht gefunden
```


Now, a subset of the data (every second observation) is used for training.


```r
mod <- train(lrn, task, subset = seq(from = 1, to = nrow(iris), by = 2))
```

```
## Error: cannot coerce class "c("classif.rpart", "RLearnerClassif",
## "RLearner", "Learner")" to a data.frame
```

```r
mod
```

```
## Error: Objekt 'mod' nicht gefunden
```

  
It's still simple to access the wrapped rpart model, but in most cases there won't be a need to do so.


```r
mod$learner.model
```

```
## Error: Objekt 'mod' nicht gefunden
```



### Regression example

As a regression example we use the ``BostonHousing`` data set. Let's train a Gradient Boosting Machine and we begin with the whole data set.


```r
# Create the regression task.
library("mlbench")
data(BostonHousing)

task <- makeRegrTask(data = BostonHousing, target = "medv")
lrn <- makeLearner("regr.gbm", n.trees = 500, distribution = "laplace", interaction.depth = 3)

mod <- train(lrn, task)
```

```
## Error: cannot coerce class "c("regr.gbm", "RLearnerRegr", "RLearner",
## "Learner")" to a data.frame
```

```r
mod
```

```
## Error: Objekt 'mod' nicht gefunden
```


Now, we use a subset of the data for training (every second observation).


```r
mod <- train(lrn, task, subset = seq(1, nrow(BostonHousing), 2))
```

```
## Error: cannot coerce class "c("regr.gbm", "RLearnerRegr", "RLearner",
## "Learner")" to a data.frame
```

```r
mod
```

```
## Error: Objekt 'mod' nicht gefunden
```



