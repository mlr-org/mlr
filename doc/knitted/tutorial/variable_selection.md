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
## [selectFeatures] 2: 1 bits: mmce.test.mean= 0.3
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.48
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.02
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.02
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.02
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.02
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.02
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
## [selectFeatures] 1: 5 bits: mse.test.mean=27.5
## [selectFeatures] 1: 9 bits: mse.test.mean=43.6
## [selectFeatures] 1: 7 bits: mse.test.mean=34.6
## [selectFeatures] 1: 9 bits: mse.test.mean=21.9
## [selectFeatures] 1: 6 bits: mse.test.mean=28.2
## [selectFeatures] 1: 7 bits: mse.test.mean=22.7
## [selectFeatures] 1: 5 bits: mse.test.mean=31.4
## [selectFeatures] 1: 5 bits: mse.test.mean=26.4
## [selectFeatures] 1: 4 bits: mse.test.mean=29.4
## [selectFeatures] 1: 7 bits: mse.test.mean=  31
## [selectFeatures] 1: 5 bits: mse.test.mean=29.9
## [selectFeatures] 1: 5 bits: mse.test.mean=27.7
## [selectFeatures] 1: 3 bits: mse.test.mean=30.8
## [selectFeatures] 1: 6 bits: mse.test.mean=24.7
## [selectFeatures] 1: 8 bits: mse.test.mean=28.3
## [selectFeatures] 2: 7 bits: mse.test.mean=25.4
## [selectFeatures] 2: 5 bits: mse.test.mean=37.8
## [selectFeatures] 2: 5 bits: mse.test.mean=29.9
## [selectFeatures] 2: 5 bits: mse.test.mean=27.8
## [selectFeatures] 2: 9 bits: mse.test.mean=28.6
## [selectFeatures] 3: 6 bits: mse.test.mean=23.3
## [selectFeatures] 3: 6 bits: mse.test.mean=23.9
## [selectFeatures] 3: 4 bits: mse.test.mean=30.6
## [selectFeatures] 3: 8 bits: mse.test.mean=  44
## [selectFeatures] 3: 5 bits: mse.test.mean=27.4
## [selectFeatures] 4: 4 bits: mse.test.mean=26.6
## [selectFeatures] 4: 9 bits: mse.test.mean=22.2
## [selectFeatures] 4: 6 bits: mse.test.mean=24.1
## [selectFeatures] 4: 4 bits: mse.test.mean=30.2
## [selectFeatures] 4: 8 bits: mse.test.mean=22.7
## [selectFeatures] 5: 7 bits: mse.test.mean=22.4
## [selectFeatures] 5: 9 bits: mse.test.mean=21.9
## [selectFeatures] 5: 6 bits: mse.test.mean=  23
## [selectFeatures] 5: 7 bits: mse.test.mean=26.4
## [selectFeatures] 5: 7 bits: mse.test.mean=27.6
## [selectFeatures] 6: 8 bits: mse.test.mean=22.3
## [selectFeatures] 6: 9 bits: mse.test.mean=22.7
## [selectFeatures] 6: 9 bits: mse.test.mean=22.2
## [selectFeatures] 6: 9 bits: mse.test.mean=21.9
## [selectFeatures] 6: 8 bits: mse.test.mean=23.3
## [selectFeatures] 7: 8 bits: mse.test.mean=23.3
## [selectFeatures] 7: 9 bits: mse.test.mean=21.9
## [selectFeatures] 7: 9 bits: mse.test.mean=21.9
## [selectFeatures] 7: 10 bits: mse.test.mean=21.8
## [selectFeatures] 7: 7 bits: mse.test.mean=22.3
## [selectFeatures] 8: 9 bits: mse.test.mean=21.9
## [selectFeatures] 8: 9 bits: mse.test.mean=21.9
## [selectFeatures] 8: 10 bits: mse.test.mean=21.3
## [selectFeatures] 8: 10 bits: mse.test.mean=21.8
## [selectFeatures] 8: 9 bits: mse.test.mean=21.9
## [selectFeatures] 9: 11 bits: mse.test.mean=20.8
## [selectFeatures] 9: 9 bits: mse.test.mean=22.6
## [selectFeatures] 9: 9 bits: mse.test.mean=21.9
## [selectFeatures] 9: 10 bits: mse.test.mean=21.8
## [selectFeatures] 9: 8 bits: mse.test.mean=24.6
## [selectFeatures] 10: 9 bits: mse.test.mean=21.9
## [selectFeatures] 10: 9 bits: mse.test.mean=24.7
## [selectFeatures] 10: 9 bits: mse.test.mean=21.9
## [selectFeatures] 10: 10 bits: mse.test.mean=20.7
## [selectFeatures] 10: 10 bits: mse.test.mean=21.8
```

```r
sfGA
```

```
## FeatSel result:
## Features (10): crim, chas, rm, age, dis, rad, tax, ptratio, b, lstat
## =20.7
```


