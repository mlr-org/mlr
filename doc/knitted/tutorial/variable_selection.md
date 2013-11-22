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
## [selectFeatures] 1: 0 bits: mmce.test.mean=0.62
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.24
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.42
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.04
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.06
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.04
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.04
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.06
```

```r
sfSeq
```

```
## FeatSel result:
## Features (1): Petal.Length
## =0.04
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
## [selectFeatures] 1: 5 bits: mse.test.mean=33.3
## [selectFeatures] 1: 4 bits: mse.test.mean=67.1
## [selectFeatures] 1: 7 bits: mse.test.mean=32.8
## [selectFeatures] 1: 7 bits: mse.test.mean=27.8
## [selectFeatures] 1: 6 bits: mse.test.mean=62.2
## [selectFeatures] 1: 8 bits: mse.test.mean=33.5
## [selectFeatures] 1: 6 bits: mse.test.mean=33.5
## [selectFeatures] 1: 7 bits: mse.test.mean=24.7
## [selectFeatures] 1: 8 bits: mse.test.mean=26.7
## [selectFeatures] 1: 6 bits: mse.test.mean=38.2
## [selectFeatures] 1: 7 bits: mse.test.mean=33.9
## [selectFeatures] 1: 8 bits: mse.test.mean=29.3
## [selectFeatures] 1: 9 bits: mse.test.mean=26.7
## [selectFeatures] 1: 7 bits: mse.test.mean=31.7
## [selectFeatures] 1: 8 bits: mse.test.mean=33.5
## [selectFeatures] 2: 8 bits: mse.test.mean=26.8
## [selectFeatures] 2: 6 bits: mse.test.mean=29.6
## [selectFeatures] 2: 4 bits: mse.test.mean=34.2
## [selectFeatures] 2: 5 bits: mse.test.mean=30.9
## [selectFeatures] 2: 9 bits: mse.test.mean=26.7
## [selectFeatures] 3: 4 bits: mse.test.mean=28.9
## [selectFeatures] 3: 8 bits: mse.test.mean=28.7
## [selectFeatures] 3: 8 bits: mse.test.mean=26.6
## [selectFeatures] 3: 4 bits: mse.test.mean=30.8
## [selectFeatures] 3: 4 bits: mse.test.mean=30.8
## [selectFeatures] 4: 8 bits: mse.test.mean=  23
## [selectFeatures] 4: 5 bits: mse.test.mean=27.5
## [selectFeatures] 4: 7 bits: mse.test.mean=53.1
## [selectFeatures] 4: 7 bits: mse.test.mean=30.3
## [selectFeatures] 4: 5 bits: mse.test.mean=34.1
## [selectFeatures] 5: 8 bits: mse.test.mean=28.7
## [selectFeatures] 5: 9 bits: mse.test.mean=30.9
## [selectFeatures] 5: 6 bits: mse.test.mean=31.5
## [selectFeatures] 5: 9 bits: mse.test.mean=  22
## [selectFeatures] 5: 8 bits: mse.test.mean=25.4
## [selectFeatures] 6: 7 bits: mse.test.mean=23.5
## [selectFeatures] 6: 8 bits: mse.test.mean=26.6
## [selectFeatures] 6: 9 bits: mse.test.mean=22.9
## [selectFeatures] 6: 7 bits: mse.test.mean=27.7
## [selectFeatures] 6: 8 bits: mse.test.mean=26.6
## [selectFeatures] 7: 6 bits: mse.test.mean=26.7
## [selectFeatures] 7: 9 bits: mse.test.mean=24.5
## [selectFeatures] 7: 6 bits: mse.test.mean=31.3
## [selectFeatures] 7: 7 bits: mse.test.mean=24.7
## [selectFeatures] 7: 9 bits: mse.test.mean=27.3
## [selectFeatures] 8: 7 bits: mse.test.mean=23.1
## [selectFeatures] 8: 9 bits: mse.test.mean=26.7
## [selectFeatures] 8: 6 bits: mse.test.mean=25.7
## [selectFeatures] 8: 9 bits: mse.test.mean=23.7
## [selectFeatures] 8: 8 bits: mse.test.mean=24.7
## [selectFeatures] 9: 8 bits: mse.test.mean=24.7
## [selectFeatures] 9: 10 bits: mse.test.mean=22.1
## [selectFeatures] 9: 6 bits: mse.test.mean=24.9
## [selectFeatures] 9: 8 bits: mse.test.mean=22.1
## [selectFeatures] 9: 8 bits: mse.test.mean=26.6
## [selectFeatures] 10: 9 bits: mse.test.mean=  25
## [selectFeatures] 10: 8 bits: mse.test.mean=24.5
## [selectFeatures] 10: 9 bits: mse.test.mean=23.4
## [selectFeatures] 10: 6 bits: mse.test.mean=24.9
## [selectFeatures] 10: 7 bits: mse.test.mean=23.2
```

```r
sfGA
```

```
## FeatSel result:
## Features (9): crim, indus, rm, age, dis, tax, ptratio, b, lstat
## =  22
```


