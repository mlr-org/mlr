Feature Selection
==================

Often, data sets include a great amount of variables and you want to reduce them. This technique of selecting a subset of relevant variables is called variable selection. Variable selection can make the model interpretable, the learning process faster and the fitted model more general by removing irrelevant variables.  Different approaches exist, in order to figure out, which the relevant variables are.


Quick start
-----------

### Classification example

Let's train a decision tree on the ``iris`` data and use a sequential forward search to find the best group of features w.r.t. the **mmce** (mean misclassification error).


```r
library("mlr")
task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.rpart")
rdesc <- makeResampleDesc("Holdout")

ctrlSeq <- makeFeatSelControlSequential(method = "sfs")
sfSeq <- selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlSeq)
```

```
## [selectFeatures] 1: 0 bits: mmce.test.mean=0.68
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.28
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.46
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.02
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.06
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.02
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.02
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.06
```

```r
sfSeq
```

```
## FeatSel result:
## Features (1): Petal.Length
## =0.02
```



### Regression example

We fit a simple linear regression model to the ``BostonHousing`` data set and use a genetic algorithm to find a feature set that reduces the **mse** (mean squared error).


```r
library("mlbench")
data(BostonHousing)

task <- makeRegrTask(data = BostonHousing, target = "medv")
lrn <- makeLearner("regr.lm")
rdesc <- makeResampleDesc("Holdout")

ctrlGA <- makeFeatSelControlGA(maxit = 10)
sfGA <- selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlGA)
```

```
## [selectFeatures] 1: 3 bits: mse.test.mean=35.9
## [selectFeatures] 1: 7 bits: mse.test.mean=27.9
## [selectFeatures] 1: 7 bits: mse.test.mean=32.2
## [selectFeatures] 1: 6 bits: mse.test.mean=58.7
## [selectFeatures] 1: 6 bits: mse.test.mean=36.6
## [selectFeatures] 1: 8 bits: mse.test.mean=35.1
## [selectFeatures] 1: 8 bits: mse.test.mean=33.6
## [selectFeatures] 1: 8 bits: mse.test.mean=27.4
## [selectFeatures] 1: 9 bits: mse.test.mean=  30
## [selectFeatures] 1: 9 bits: mse.test.mean=32.9
## [selectFeatures] 1: 7 bits: mse.test.mean=35.1
## [selectFeatures] 1: 9 bits: mse.test.mean=32.9
## [selectFeatures] 1: 7 bits: mse.test.mean=30.6
## [selectFeatures] 1: 10 bits: mse.test.mean=27.6
## [selectFeatures] 1: 7 bits: mse.test.mean=  27
## [selectFeatures] 2: 9 bits: mse.test.mean=  25
## [selectFeatures] 2: 7 bits: mse.test.mean=28.2
## [selectFeatures] 2: 9 bits: mse.test.mean=26.9
## [selectFeatures] 2: 7 bits: mse.test.mean=29.6
## [selectFeatures] 2: 8 bits: mse.test.mean=25.5
## [selectFeatures] 3: 9 bits: mse.test.mean=25.6
## [selectFeatures] 3: 9 bits: mse.test.mean=27.6
## [selectFeatures] 3: 7 bits: mse.test.mean=28.5
## [selectFeatures] 3: 8 bits: mse.test.mean=27.5
## [selectFeatures] 3: 10 bits: mse.test.mean=27.2
## [selectFeatures] 4: 12 bits: mse.test.mean=24.4
## [selectFeatures] 4: 9 bits: mse.test.mean=  25
## [selectFeatures] 4: 9 bits: mse.test.mean=25.6
## [selectFeatures] 4: 7 bits: mse.test.mean=28.9
## [selectFeatures] 4: 8 bits: mse.test.mean=27.5
## [selectFeatures] 5: 11 bits: mse.test.mean=24.7
## [selectFeatures] 5: 10 bits: mse.test.mean=25.3
## [selectFeatures] 5: 10 bits: mse.test.mean=24.3
## [selectFeatures] 5: 9 bits: mse.test.mean=31.2
## [selectFeatures] 5: 9 bits: mse.test.mean=29.7
## [selectFeatures] 6: 10 bits: mse.test.mean=25.8
## [selectFeatures] 6: 9 bits: mse.test.mean=26.2
## [selectFeatures] 6: 10 bits: mse.test.mean=25.1
## [selectFeatures] 6: 10 bits: mse.test.mean=30.6
## [selectFeatures] 6: 8 bits: mse.test.mean=27.5
## [selectFeatures] 7: 9 bits: mse.test.mean=25.6
## [selectFeatures] 7: 9 bits: mse.test.mean=26.7
## [selectFeatures] 7: 11 bits: mse.test.mean=24.7
## [selectFeatures] 7: 9 bits: mse.test.mean=30.3
## [selectFeatures] 7: 11 bits: mse.test.mean=24.4
## [selectFeatures] 8: 8 bits: mse.test.mean=25.9
## [selectFeatures] 8: 12 bits: mse.test.mean=25.1
## [selectFeatures] 8: 10 bits: mse.test.mean=25.7
## [selectFeatures] 8: 12 bits: mse.test.mean=24.8
## [selectFeatures] 8: 11 bits: mse.test.mean=  25
## [selectFeatures] 9: 10 bits: mse.test.mean=24.3
## [selectFeatures] 9: 10 bits: mse.test.mean=27.8
## [selectFeatures] 9: 10 bits: mse.test.mean=24.3
## [selectFeatures] 9: 11 bits: mse.test.mean=24.5
## [selectFeatures] 9: 11 bits: mse.test.mean=24.4
## [selectFeatures] 10: 12 bits: mse.test.mean=24.4
## [selectFeatures] 10: 10 bits: mse.test.mean=25.1
## [selectFeatures] 10: 10 bits: mse.test.mean=26.3
## [selectFeatures] 10: 11 bits: mse.test.mean=24.4
## [selectFeatures] 10: 9 bits: mse.test.mean=24.9
```

```r
sfGA
```

```
## FeatSel result:
## Features (10): crim, zn, nox, rm, dis, rad, tax, ptratio, b, lstat
## =24.3
```


