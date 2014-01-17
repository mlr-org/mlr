


Resampling
==========

In order to assess the performance of a learning algorithm, usually resampling 
strategies are used. There are various methods how this can be done, e.g.
cross-validation and bootstrap just to mention two popular approaches. 
In **mlr**, the [resample](http://berndbischl.github.io/mlr/man/resample.html)-function, depending on the chosen resampling strategy, 
fits the selected learner using the corresponding training sets and performs 
predictions for the corresponding training/test sets and calculates the chosen
performance measures.


Quick start
-----------

### Classification example



```splus
library("mlr")

## Define a learning task and an appropriate learner
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")

## Perform a 3-fold cross-validation
rdesc = makeResampleDesc("CV", iters = 3)
r = resample(lrn, task, rdesc)
```

```
## Loading packages on slaves: mlr
## [Resample] cross-validation iter: 1
## [Resample] cross-validation iter: 2
## [Resample] cross-validation iter: 3
## [Resample] Result: mmce.test.mean=0.02
```

```splus
r
```

```
## $measures.train
##   iter mmce
## 1    1   NA
## 2    2   NA
## 3    3   NA
## 
## $measures.test
##   iter mmce
## 1    1 0.00
## 2    2 0.04
## 3    3 0.02
## 
## $aggr
## mmce.test.mean 
##           0.02 
## 
## $pred
## Resampled Prediction for:
## Resample description: cross-validation with 3 iterations.
## Predict: test
## Stratification: FALSE
## predict.type: response
## threshold: 
## time (mean): 0.00
## 'data.frame':	150 obs. of  5 variables:
##  $ id      : int  4 6 7 9 11 14 15 16 17 20 ...
##  $ truth   : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ response: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ iter    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ set     : Factor w/ 1 level "test": 1 1 1 1 1 1 1 1 1 1 ...
## 
## $models
## NULL
## 
## $extract
## $extract[[1]]
## NULL
## 
## $extract[[2]]
## NULL
## 
## $extract[[3]]
## NULL
```


In this example, the peformance measure is mmce (mean misclassification error), 
the default for classification. See the documentation for [measures](http://berndbischl.github.io/mlr/man/measures.html) for a 
complete list of performance measures available in **mlr**. More explanations, 
concerning the evaluation of learning methods, are given in section [Evaluating Learner Performance](performance.md).


### Regression example

As a regression example, the ``BostonHousing`` data set is used again.


```splus
library("mlr")
library("mlbench")
data(BostonHousing)

## Define a learning task and an appropriate learner
task = makeRegrTask(data = BostonHousing, target = "medv")
lrn = makeLearner("regr.lm")

## Perform a 3-fold cross-validation
rdesc = makeResampleDesc("CV", iters = 3)
r = resample(lrn, task, rdesc)
```

```
## Loading packages on slaves: mlr
## [Resample] cross-validation iter: 1
## [Resample] cross-validation iter: 2
## [Resample] cross-validation iter: 3
## [Resample] Result: mse.test.mean=23.7
```

```splus
r
```

```
## $measures.train
##   iter mse
## 1    1  NA
## 2    2  NA
## 3    3  NA
## 
## $measures.test
##   iter   mse
## 1    1 21.39
## 2    2 21.70
## 3    3 28.11
## 
## $aggr
## mse.test.mean 
##         23.73 
## 
## $pred
## Resampled Prediction for:
## Resample description: cross-validation with 3 iterations.
## Predict: test
## Stratification: FALSE
## predict.type: response
## threshold: 
## time (mean): 0.00
## 'data.frame':	506 obs. of  5 variables:
##  $ id      : int  2 7 8 10 11 15 23 29 31 32 ...
##  $ truth   : num  21.6 22.9 27.1 18.9 15 18.2 15.2 18.4 12.7 14.5 ...
##  $ response: num  25.2 22.9 19.5 18.9 19 ...
##  $ iter    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ set     : Factor w/ 1 level "test": 1 1 1 1 1 1 1 1 1 1 ...
## 
## $models
## NULL
## 
## $extract
## $extract[[1]]
## NULL
## 
## $extract[[2]]
## NULL
## 
## $extract[[3]]
## NULL
```


For regression, the default performance measure is mse (mean squared error).


Further information
-------------------

Resampling strategies concern the process of sampling new data sets
from your data set *D* under examination. One wants to generate
various training and test sets, which the learning method can be
fitted and validated on. Here it is assumed that every resampling
strategy consists of a couple of iterations and for each one of them
exist indices into *D*, defining the respective training and test
sets. 
These iterations are implemented by storing the index set in a
so called [ResampleInstance](http://berndbischl.github.io/mlr/man/makeResampleInstance.html) object. The reasons for having the user
create this data explicitly and not just set an option in an R function
to choose the resampling method are:

* It is easier to create paired experiments, where you train and test
  different methods on exactly the same sets, especially when you want
  to add another method to a comparison experiment you already did.
* It is easy to add other resampling methods later on. You can
  simply use S4 inheritance, derived from the [ResampleInstance](http://berndbischl.github.io/mlr/man/makeResampleInstance.html)
  class, but you do not have to touch any methods that use the
  resampling strategy.

![Resampling Figure](http://mlr.r-forge.r-project.org/_images/resampling.png "Resampling Figure")
                     
                     
Resample descriptions and resample instances
--------------------------------------------

There are two types of objects: a [ResampleDesc](http://berndbischl.github.io/mlr/man/makeResampleDesc.html) object, which stands for
a resample description, e.g. a 10-fold cross-validation and a [ResampleInstance](http://berndbischl.github.io/mlr/man/makeResampleInstance.html)
object, which creates a resample instance for a specific task and based on a resample
description. These can be generated by means of the factory methods [makeResampleDesc](http://berndbischl.github.io/mlr/man/makeResampleDesc.html)
and make[ResampleInstance](http://berndbischl.github.io/mlr/man/makeResampleInstance.html).

For every resampling strategy, there is a description class, inheriting
from [ResampleDesc](http://berndbischl.github.io/mlr/man/makeResampleDesc.html) (which completely characterizes the necessary
parameters), as well as a class inheriting from [ResampleInstance](http://berndbischl.github.io/mlr/man/makeResampleInstance.html). This latter
class takes the description object and takes care of the random
drawing of indices. While this seems overly complicated, it is
necessary as sometimes one only wants to describe the drawing process,
while in other instances one wants to create the specific index
sets. Also, there are convenient methods to make the construction
process as easy as possible.


```splus
## get the cv.instance directly
rdesc = makeResampleDesc("CV", iters = 10)
rinst = makeResampleInstance(rdesc, size = nrow(iris))
```


Asking the [ResampleDesc](http://berndbischl.github.io/mlr/man/makeResampleDesc.html) or [ResampleInstance](http://berndbischl.github.io/mlr/man/makeResampleInstance.html) objects for further
information is easy, just inspect the list elements by using the $-operator.


```splus
## description object number of iterations
rdesc$iters

## resample.instance object number of iterations
rinst$desc$iters

rinst$train.inds[[3]]
rinst$test.inds[[3]]
```


Please refer to the help pages of the specific classes for a complete
list of getters.


Included resampling strategies
------------------------------

The package comes with a couple of predefined strategies.

* 'Subsample' for subsampling,
* 'Holdout' for holdout (training/test),
* 'CV' for cross-validation, 
* 'LOO' for leave-one-out, 
* 'StratCV' for stratified cross-validation, 
* 'RepCV' for repeated cross-validation,
* 'Bootstrap' for out-of-bag bootstrap.



### Subsampling

In each iteration *i* the data set *D* is randomly partitioned into a
training and a test set according to a given percentage, e.g. 2/3
training and 1/3 test set. If there is just one iteration, the strategy
is commonly called `holdout` or `test sample estimation`.


```splus
rdesc = makeResampleDesc("Subsample", iters = 10, split = 2/3)
rdesc = makeResampleDesc("Subsample", iters = 1, split = 2/3)
```


### `k`-fold cross-validation

The data set is partitioned into *k* subsets of (nearly) equal size. 
In the *i*-th step of the *k* iterations the *i*-th subset is 
used as a test set, while the remaining parts form the training set.


```splus
rdesc = makeResampleDesc("CV", iters = 10)
```


### Bootstrapping

*B* new data sets *D_1* to *D_B* are drawn from
*D* with replacement, each of the same size as *D*. 
In the *i*-th iteration *D_i* forms the training set, 
while the remaining elements from *D*, i.e. elements not 
occuring in the training set, form the test set.


```splus
rdesc = makeResampleDesc("Bootstrap", iters = 10)
```


<!--(
                     |resampling_desc_figure|

                     |resampling_nested_resampling_figure|
)-->

Evaluation of Predictions after Resampling
------------------------------------------

The [resample](http://berndbischl.github.io/mlr/man/resample.html) function evaluates the performance of your learner using
a certain resampling strategy for a given machine learning task.

When you use a resampling strategy, **mlr** offers the following
possibilities to evaluate your predictions: Every single resampling
iteration is handled as described in the explanation above, i.e. you
train a model on a training part of the data set, predict on test set
and compare predicted and true label w.r.t. some performance
measure. This is proceeded for every iteration so that you have
e.g. ten performance values in the case of 10-fold cross-validation.
The question arises, how to aggregate those values. You can specify
that explicitly, the default is the mean. Let's have a look at an
example...


### Classification example

For the example code, we use the standard ``iris`` data set and compare a 
Decision Tree and the Linear Discriminant Analysis based on a 3-fold
cross-validation:


```splus
## Classification task
task = makeClassifTask(data = iris, target = "Species")

## Resample instance for Cross-validation
rdesc = makeResampleDesc("CV", iters = 3)
rinst = makeResampleInstance(rdesc, task = task)
```


Now, we fit a decision tree for each fold and measure both the mean misclassification 
error (`mmce`) and the accuracy (`acc`):


```splus
## Merge learner (lrn), i.e. Decision Tree, classification task (task) and
## resample instance (rinst)
lrn = makeLearner("classif.rpart")
r1 = resample(lrn, task, rinst, list(mmce, acc))
```

```
## Loading packages on slaves: mlr
## [Resample] cross-validation iter: 1
## [Resample] cross-validation iter: 2
## [Resample] cross-validation iter: 3
## [Resample] Result: mmce.test.mean=0.0667,acc.test.mean=0.933
```


Let's set a couple of hyperparameters for rpart


```splus
lrn1 = makeLearner("classif.rpart", minsplit = 10, cp = 0.03)
r1 = resample(lrn1, task, rinst, list(mmce, acc))
```

```
## Loading packages on slaves: mlr
## [Resample] cross-validation iter: 1
## [Resample] cross-validation iter: 2
## [Resample] cross-validation iter: 3
## [Resample] Result: mmce.test.mean=0.0667,acc.test.mean=0.933
```

```splus


## Second resample for LDA as learner
lrn2 = makeLearner("classif.lda")
r2 = resample(lrn2, task, rinst, list(mmce, acc))
```

```
## Loading packages on slaves: mlr
## [Resample] cross-validation iter: 1
## [Resample] cross-validation iter: 2
## [Resample] cross-validation iter: 3
## [Resample] Result: mmce.test.mean=0.02,acc.test.mean=0.98
```

```splus

## Let's see how well both classifiers did w.r.t mean misclassification error
## and accuracy
r1
```

```
## $measures.train
##   iter mmce acc
## 1    1   NA  NA
## 2    2   NA  NA
## 3    3   NA  NA
## 
## $measures.test
##   iter mmce  acc
## 1    1 0.12 0.88
## 2    2 0.06 0.94
## 3    3 0.02 0.98
## 
## $aggr
## mmce.test.mean  acc.test.mean 
##        0.06667        0.93333 
## 
## $pred
## Resampled Prediction for:
## Resample description: cross-validation with 3 iterations.
## Predict: test
## Stratification: FALSE
## predict.type: response
## threshold: 
## time (mean): 0.00
## 'data.frame':	150 obs. of  5 variables:
##  $ id      : int  7 8 9 10 11 18 21 27 28 29 ...
##  $ truth   : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ response: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ iter    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ set     : Factor w/ 1 level "test": 1 1 1 1 1 1 1 1 1 1 ...
## 
## $models
## NULL
## 
## $extract
## $extract[[1]]
## NULL
## 
## $extract[[2]]
## NULL
## 
## $extract[[3]]
## NULL
```

```splus
r2
```

```
## $measures.train
##   iter mmce acc
## 1    1   NA  NA
## 2    2   NA  NA
## 3    3   NA  NA
## 
## $measures.test
##   iter mmce  acc
## 1    1 0.06 0.94
## 2    2 0.00 1.00
## 3    3 0.00 1.00
## 
## $aggr
## mmce.test.mean  acc.test.mean 
##           0.02           0.98 
## 
## $pred
## Resampled Prediction for:
## Resample description: cross-validation with 3 iterations.
## Predict: test
## Stratification: FALSE
## predict.type: response
## threshold: 
## time (mean): 0.00
## 'data.frame':	150 obs. of  5 variables:
##  $ id      : int  7 8 9 10 11 18 21 27 28 29 ...
##  $ truth   : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ response: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ iter    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ set     : Factor w/ 1 level "test": 1 1 1 1 1 1 1 1 1 1 ...
## 
## $models
## NULL
## 
## $extract
## $extract[[1]]
## NULL
## 
## $extract[[2]]
## NULL
## 
## $extract[[3]]
## NULL
```


If we want to see the individual values for each fold on the test set, we can access 
the `measures.test` element of the result list:


```splus
r1$measures.test
```

```
##   iter mmce  acc
## 1    1 0.12 0.88
## 2    2 0.06 0.94
## 3    3 0.02 0.98
```


As you can see above, in every fold of the 3-fold cross-validation you
get one mean misclassification error (cf. the second column).

The aggregated performance values are stored in the `aggr` list element:


```splus
r1$aggr
```

```
## mmce.test.mean  acc.test.mean 
##        0.06667        0.93333
```


The latter value is the aggregation, i.e. by default the mean, of the three 
misclassification errors from the table above.
Having a look at the single losses is of course possible as well.

### Regression example

Now, we use the ``BostonHousing`` data and compare the results of a Neural Net and a k-Nearest-Neighbor regression using out-of-bag bootstraping.


```splus
library("mlr")
library("mlbench")
data(BostonHousing)

## Regression task
task = makeRegrTask(data = BostonHousing, target = "medv")

## Resample instance for bootstraping
rdesc = makeResampleDesc("Bootstrap", iters = 3)
rinst = makeResampleInstance(rdesc, task = task)

ms = list(mse, medae)

lrn1 = makeLearner("regr.nnet")
r1 = resample(lrn1, task, rinst, measures = ms)
```

```
## Loading packages on slaves: mlr
## [Resample] OOB bootstrapping iter: 1
## [Resample] OOB bootstrapping iter: 2
## [Resample] OOB bootstrapping iter: 3
## [Resample] Result: mse.test.mean=73.8,medae.test.mean=3.91
```

```splus

## Another resampling for the k-Nearest Neighbor regression
lrn2 = makeLearner("regr.kknn")
r2 = resample(lrn2, task, rinst, measures = ms)
```

```
## Loading packages on slaves: mlr
## [Resample] OOB bootstrapping iter: 1
## [Resample] OOB bootstrapping iter: 2
## [Resample] OOB bootstrapping iter: 3
## [Resample] Result: mse.test.mean=27.3,medae.test.mean=1.73
```


Now, we can compare both methods regarding the **mse** (mean squared error) and the
**medae** (median of absolute errors):


```splus
r1$measures.test
```

```
##   iter    mse medae
## 1    1  81.50 4.315
## 2    2  30.79 2.222
## 3    3 108.99 5.195
```

```splus
r2$measures.test
```

```
##   iter   mse medae
## 1    1 29.04 1.664
## 2    2 26.20 1.892
## 3    3 26.71 1.644
```



<!--( 

.. |resampling_figure| image:: /_images/resampling.png
     :align: middle
     :width: 40em
     :alt: Resampling illustration
     
.. |resampling_desc_figure| image:: /_images/resampling_desc_and_instance.png
     :align: middle
     :width: 40em
     :alt: Resampling description and instance
     
.. |nested_resampling_figure| image:: /_images/nested_resampling.png
     :align: middle
     :width: 60em
     :alt: Nested resampling illustration
     
)-->
