Evaluating Learner Performance
===============================

The quality of predictions in **mlr** can be assessed w.r.t. some
performance measure.

Typical performance measures are **mean misclassification error** (mmce),
**accuracy** (acc) or the ones based on ROC analysis for classification and **mean
of squared errors** (mse) or **mean of absolute errors** (mae) for regression. 
It is also possible to access the time to train the model,
the time to compute the prediction and their sum as performance
measures.

To see which performance measures are implemented, have a look at [measures](http://berndbischl.github.io/mlr/man/measures.html). If you want 
to implement an additional measure or include a measure with non-standard 
misclassification costs, go to the section [create_measure](create_measure.md). In order to calculate 
the performance measures, the function [performance](http://berndbischl.github.io/mlr/man/performance.html) is used.


Classification example
----------------------

We fit a Linear Discriminant Analysis on a subset of the ``iris`` data set and calculate
the mean misclassification error (mmce) on the test data set.


```splus
library("mlr")

task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
mod = train(lrn, task = task, subset = seq(1, 150, 2))
pred = predict(mod, task = task, subset = seq(2, 150, 2))

performance(pred, measures = mmce)
```

```
## [1] 0.04
```

  
Let's have a look at some more performance measures. Note that, in order to assess 
the time needed for training, the fitted model has to be passed.


```splus
performance(pred, measures = acc)
```

```
## [1] 0.96
```

```splus

performance(pred = pred, measures = timepredict)
```

```
## [1] 0.003
```

```splus

performance(pred = pred, measures = timetrain, model = mod)
```

```
## [1] 0.008
```

```splus
performance(pred = pred, measures = timeboth, model = mod)
```

```
## [1] 0.011
```


Of course we can also calculate multiple performance measures at once simply by unsing a list of meassures which
can also include [your own measure](create_measure.md).


```splus
ms = list(mmce = mmce, acc = acc, timetrain = timetrain, timeboth = timeboth)
performance(pred = pred, measures = ms, model = mod)
```

```
##      mmce       acc timetrain  timeboth 
##     0.040     0.960     0.008     0.011
```


Binary classification
---------------------

In the two-class case many more measures are available. In the following example,
the accuracy, as well as the false positive and false negative rates are computed.


```splus
library("mlbench")
data(Sonar)

task = makeClassifTask(data = Sonar, target = "Class", positive = "M")
lrn = makeLearner("classif.rpart")
mod = train(lrn, task = task)
pred = predict(mod, task = task)

performance(pred, measures = acc)
```

```
## [1] 0.875
```

```splus
performance(pred, measures = fpr)
```

```
## [1] 0.1031
```

```splus
performance(pred, measures = fnr)
```

```
## [1] 0.1441
```



Note that, in order to calculate the AUC, the area under the ROC (receiver 
operating characteristic) curve, we have to make sure that posterior
probabilities are predicted, i.e. set the predict type of the [Learner](http://berndbischl.github.io/mlr/man/makeLearner.html) to "prob".


```splus
library("mlbench")
data(Sonar)

task = makeClassifTask(data = Sonar, target = "Class", positive = "M")
lrn = makeLearner("classif.rpart", predict.type = "prob")
mod = train(lrn, task = task)
pred = predict(mod, task = task)

performance(pred, measures = auc)
```

```
## KernSmooth 2.23 loaded
## Copyright M. P. Wand 1997-2009
```

```
## [1] 0.9224
```


For more information on ROC analysis see section [ROC Analysis](roc_analysis.md).


Regression example
------------------

In regression, everything works analogous to the above examples.
We again use the ``BostonHousing`` data set, fit a Gradient Boosting Machine on a
training set and calculate the mean of squared errors and the mean of absolute 
errors on the test data set.


```splus
library(mlbench)
data(BostonHousing)

task = makeRegrTask(data = BostonHousing, target = "medv")

## Training and test set indices
training.set = seq(from = 1, to = nrow(BostonHousing), by = 2)
test.set = seq(from = 2, to = nrow(BostonHousing), by = 2)

## Gradient Boosting Machine on training set
lrn = makeLearner("regr.gbm", n.trees = 1000)
mod = train(lrn, task, subset = training.set)

## Prediction on test set data
pred = predict(mod, newdata = BostonHousing[test.set, ])

## Compare predicted and true labels using measures MSE and MAE
ms = list(mse = mse, mae = mae)
sapply(ms, function(meas) performance(pred, measures = meas))
```

```
##    mse    mae 
## 42.913  4.557
```



