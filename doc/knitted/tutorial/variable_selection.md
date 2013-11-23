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
## [selectFeatures] 1: 0 bits: mmce.test.mean=0.66
## [selectFeatures] 2: 1 bits: mmce.test.mean= 0.2
## [selectFeatures] 2: 1 bits: mmce.test.mean= 0.4
## [selectFeatures] 2: 1 bits: mmce.test.mean=   0
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.06
## [selectFeatures] 3: 2 bits: mmce.test.mean=   0
## [selectFeatures] 3: 2 bits: mmce.test.mean=   0
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.06
```

```r
sfSeq
```

```
## FeatSel result:
## Features (1): Petal.Length
## =   0
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
## [selectFeatures] 1: 5 bits: mse.test.mean=28.7
## [selectFeatures] 1: 6 bits: mse.test.mean=25.5
## [selectFeatures] 1: 7 bits: mse.test.mean=23.7
## [selectFeatures] 1: 6 bits: mse.test.mean=40.1
## [selectFeatures] 1: 5 bits: mse.test.mean=35.3
## [selectFeatures] 1: 5 bits: mse.test.mean=38.2
## [selectFeatures] 1: 5 bits: mse.test.mean=34.1
## [selectFeatures] 1: 8 bits: mse.test.mean=24.9
## [selectFeatures] 1: 12 bits: mse.test.mean=22.3
## [selectFeatures] 1: 7 bits: mse.test.mean=26.9
## [selectFeatures] 1: 6 bits: mse.test.mean=27.5
## [selectFeatures] 1: 8 bits: mse.test.mean=32.7
## [selectFeatures] 1: 6 bits: mse.test.mean=  31
## [selectFeatures] 1: 4 bits: mse.test.mean=35.2
## [selectFeatures] 1: 7 bits: mse.test.mean=26.9
## [selectFeatures] 2: 8 bits: mse.test.mean=  31
## [selectFeatures] 2: 9 bits: mse.test.mean=  30
## [selectFeatures] 2: 11 bits: mse.test.mean=22.2
## [selectFeatures] 2: 7 bits: mse.test.mean=27.8
## [selectFeatures] 2: 7 bits: mse.test.mean=24.1
## [selectFeatures] 3: 8 bits: mse.test.mean=  27
## [selectFeatures] 3: 7 bits: mse.test.mean=33.2
## [selectFeatures] 3: 8 bits: mse.test.mean=26.4
## [selectFeatures] 3: 6 bits: mse.test.mean=31.3
## [selectFeatures] 3: 7 bits: mse.test.mean=24.1
## [selectFeatures] 4: 8 bits: mse.test.mean=37.1
## [selectFeatures] 4: 8 bits: mse.test.mean=24.9
## [selectFeatures] 4: 9 bits: mse.test.mean=20.6
## [selectFeatures] 4: 7 bits: mse.test.mean=24.1
## [selectFeatures] 4: 9 bits: mse.test.mean=22.5
## [selectFeatures] 5: 8 bits: mse.test.mean=24.9
## [selectFeatures] 5: 8 bits: mse.test.mean=20.7
## [selectFeatures] 5: 8 bits: mse.test.mean=23.7
## [selectFeatures] 5: 10 bits: mse.test.mean=27.8
## [selectFeatures] 5: 8 bits: mse.test.mean=26.9
## [selectFeatures] 6: 7 bits: mse.test.mean=24.1
## [selectFeatures] 6: 7 bits: mse.test.mean=24.1
## [selectFeatures] 6: 6 bits: mse.test.mean=22.2
## [selectFeatures] 6: 8 bits: mse.test.mean=27.7
## [selectFeatures] 6: 8 bits: mse.test.mean=23.7
## [selectFeatures] 7: 8 bits: mse.test.mean=23.7
## [selectFeatures] 7: 8 bits: mse.test.mean=20.7
## [selectFeatures] 7: 7 bits: mse.test.mean=22.3
## [selectFeatures] 7: 8 bits: mse.test.mean=23.7
## [selectFeatures] 7: 8 bits: mse.test.mean=23.7
## [selectFeatures] 8: 7 bits: mse.test.mean=25.1
## [selectFeatures] 8: 6 bits: mse.test.mean=  25
## [selectFeatures] 8: 11 bits: mse.test.mean=22.2
## [selectFeatures] 8: 8 bits: mse.test.mean=20.7
## [selectFeatures] 8: 8 bits: mse.test.mean=20.7
## [selectFeatures] 9: 8 bits: mse.test.mean=25.5
## [selectFeatures] 9: 11 bits: mse.test.mean=22.2
## [selectFeatures] 9: 9 bits: mse.test.mean=25.5
## [selectFeatures] 9: 9 bits: mse.test.mean=20.6
## [selectFeatures] 9: 11 bits: mse.test.mean=23.6
## [selectFeatures] 10: 10 bits: mse.test.mean=22.3
## [selectFeatures] 10: 7 bits: mse.test.mean=20.3
## [selectFeatures] 10: 7 bits: mse.test.mean=20.7
## [selectFeatures] 10: 9 bits: mse.test.mean=19.1
## [selectFeatures] 10: 10 bits: mse.test.mean=23.4
```

```r
sfGA
```

```
## FeatSel result:
## Features (9): zn, indus, nox, rm, dis, rad, ptratio, b, lstat
## =19.1
```


