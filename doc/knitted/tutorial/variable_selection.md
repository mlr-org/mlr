Feature Selection
==================

Often, data sets include a great amount of variables and you want to reduce them. 
This technique of selecting a subset of relevant variables is called variable selection. 
Variable selection can make the model interpretable, the learning process faster and the fitted model more general by removing irrelevant variables. 
Different approaches exist, in order to figure out, which the relevant variables are.
*mlr* supports [filters](#Filter) and [wrappers](#Wrapper).

Filter
------

Filters are the simplest approach to find variables that do not contain a lot of additional information and thus can be left out.
Different methods are built into **mlr**'s function `filterFeatures()` all accessing filter algorithms from the package `FSelector`.
The function is given a `task` and simply returns an importance vector.

```splus
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
importance = filterFeatures(task, method = "information.gain")
```

```
## Loading required package: FSelector
```

```splus
sort(importance, decreasing = TRUE)
```

```
##  Petal.Width Petal.Length Sepal.Length  Sepal.Width 
##       1.3784       1.3565       0.6523       0.3856
```

So according to this filter `Petal.Width` and `Petal.Length` contain the most *information*.

In a proper experimental set up you might want to automate the selection of the variables so that it can be part of the validation method of your choice.
We will use the standard 10-fold cross validation.

```splus
learner = makeLearner("classif.fnn")
learnerFiltered = makeFilterWrapper(learner = learner, fw.method = "information.gain", 
    fw.perc = 0.7)
rdesc = makeResampleDesc("CV", iters = 10)
rsres = resample(learner = learnerFiltered, task = task, resampling = rdesc, 
    show.info = FALSE, models = TRUE)
```

```
## Loading packages on slaves: mlr
```

```splus
rsres$aggr
```

```
## mmce.test.mean 
##        0.04667
```

Now you want might want to know which features have been used.
Luckily we have called `resample` with the argument `models=TRUE` which means that `rsres$models` contains a `list` of each model used for a fold.
In this case the `Learner` is also of the class `FilterWrapper` and we can call `getFilteredFeatures()` on each model.

```splus
sfeats = sapply(rsres$models, getFilteredFeatures)
table(sfeats)
```

```
## sfeats
## Petal.Length  Petal.Width Sepal.Length 
##           10           10           10
```

The selection of features seems to be very stable.
The `Sepal.Width` did not made it into a single fold.

Wrapper
-------

Unlike the **filters** *wrappers* make use of the performance a **learner** can achieve on a given subset of the features in the data.

### Quick start

#### Classification example

Let's train a decision tree on the ``iris`` data and use a sequential forward search to find the best group of features w.r.t. the **mmce** (mean misclassification error).


```splus
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.rpart")
rdesc = makeResampleDesc("Holdout")

ctrlSeq = makeFeatSelControlSequential(method = "sfs")
sfSeq = selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlSeq)
```

```
## Loading packages on slaves: mlr
## [selectFeatures] 1: 0 bits: mmce.test.mean=0.66
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.28
## Loading packages on slaves: mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.58
## Loading packages on slaves: mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.12
## Loading packages on slaves: mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.04
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.04
## Loading packages on slaves: mlr
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.04
## Loading packages on slaves: mlr
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.12
```

```splus
sfSeq
```

```
## FeatSel result:
## Features (1): Petal.Width
## =0.04
```

```splus
analyzeFeatSelResult(sfSeq, reduce = FALSE)
```

```
## FeatSel result:
## - features (1): Petal.Width
## - Performance: mmce.test.mean=0.04
## 
## Path to optimum:
## - Features: 0  	 Initial model 	 Gain: mmce.test.mean=   0 	 SELECTED
## Finished step: 1 with 	 mmce.test.mean=0.66 	 Optimum: FALSE
## - Features: 1  	 Added: Sepal.Length 	 Gain: mmce.test.mean=-0.38 	 
## - Features: 1  	 Added: Sepal.Width 	 Gain: mmce.test.mean=-0.08 	 
## - Features: 1  	 Added: Petal.Length 	 Gain: mmce.test.mean=-0.54 	 
## - Features: 1  	 Added: Petal.Width 	 Gain: mmce.test.mean=-0.62 	 SELECTED
## Finished step: 2 with 	 mmce.test.mean=0.04 	 Optimum: TRUE
## - Features: 2  	 Added: Sepal.Length 	 Gain: mmce.test.mean=   0 	 
## - Features: 2  	 Added: Sepal.Width 	 Gain: mmce.test.mean=   0 	 
## - Features: 2  	 Added: Petal.Length 	 Gain: mmce.test.mean=0.08 	 
## Finished step: 3 with 	 mmce.test.mean=0.04 	 Optimum: TRUE
## 
## Stopped, because no improving set of features (w.r.t. mmce.test.mean) was found.
```



#### Regression example

We fit a simple linear regression model to the ``BostonHousing`` data set and use a genetic algorithm to find a feature set that reduces the **mse** (mean squared error).


```splus
library("mlbench")
data(BostonHousing)

task = makeRegrTask(data = BostonHousing, target = "medv")
lrn = makeLearner("regr.lm")
rdesc = makeResampleDesc("Holdout")

ctrlGA = makeFeatSelControlGA(maxit = 10)
sfGA = selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlGA, 
    show.info = FALSE)
```

```
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
## Loading packages on slaves: mlr
```

```splus
sfGA
```

```
## FeatSel result:
## Features (11): crim, indus, nox, rm, age, dis, rad, tax, ptratio, b, ...
## =29.5
```

```splus
analyzeFeatSelResult(sfGA)
```

```
## FeatSel result:
## - features (11): crim, indus, nox, rm, age, dis, rad, tax, ptratio, b, ...
## - Performance: mse.test.mean=29.5
## 
## Path to optimum:
## Initial generation:
## - (1) 	 Features: 7  	 mse.test.mean=  41 	 Features: crim, indus, rm, age, dis, rad, ptratio 
## - (2) 	 Features: 5  	 mse.test.mean=33.4 	 Features: rm, age, ptratio, b, lstat 
## - (3) 	 Features: 8  	 mse.test.mean=37.2 	 Features: crim, zn, indus, rm, age, rad, tax, lstat 
## - (4) 	 Features: 8  	 mse.test.mean=38.3 	 Features: crim, indus, nox, rm, dis, tax, ptratio, b 
## - (5) 	 Features: 7  	 mse.test.mean=42.8 	 Features: zn, indus, nox, age, rad, b, lstat 
## - (6) 	 Features: 4  	 mse.test.mean=38.6 	 Features: indus, rm, tax, lstat 
## - (7) 	 Features: 6  	 mse.test.mean=42.5 	 Features: crim, indus, chas, nox, rm, ptratio 
## - (8) 	 Features: 8  	 mse.test.mean=  32 	 Features: crim, zn, chas, rm, dis, ptratio, b, lstat 
## - (9) 	 Features: 7  	 mse.test.mean=49.6 	 Features: zn, chas, nox, dis, rad, tax, ptratio 
## - (10) 	 Features: 8  	 mse.test.mean=41.3 	 Features: crim, indus, chas, rm, rad, tax, ptratio, b 
## Generation 1:
## - (1) 	 Features: 9  	 mse.test.mean=33.4 	 Features: crim, zn, indus, rm, age, rad, tax, ptratio, lstat 
## - (2) 	 Features: 8  	 mse.test.mean=30.7 	 Features: crim, indus, nox, rm, dis, tax, ptratio, lstat 
## - (3) 	 Features: 4  	 mse.test.mean=33.7 	 Features: rm, ptratio, b, lstat 
## Generation 2:
## - (1) 	 Features: 8  	 mse.test.mean=31.8 	 Features: crim, indus, rm, age, dis, rad, ptratio, lstat 
## - (2) 	 Features: 5  	 mse.test.mean=  34 	 Features: rm, age, rad, ptratio, lstat 
## - (3) 	 Features: 8  	 mse.test.mean=  38 	 Features: crim, zn, indus, nox, rm, age, tax, lstat 
## - (4) 	 Features: 8  	 mse.test.mean=33.7 	 Features: crim, zn, indus, rm, rad, tax, ptratio, lstat 
## Generation 3:
## - (1) 	 Features: 8  	 mse.test.mean=32.7 	 Features: crim, chas, rm, dis, tax, ptratio, b, lstat 
## - (2) 	 Features: 10  	 mse.test.mean=31.1 	 Features: crim, zn, indus, rm, age, dis, tax, ptratio, b, lstat 
## Generation 4:
## - (1) 	 Features: 8  	 mse.test.mean=31.7 	 Features: crim, indus, rm, age, dis, ptratio, b, lstat 
## - (2) 	 Features: 9  	 mse.test.mean=30.7 	 Features: crim, zn, indus, rm, dis, rad, tax, ptratio, lstat 
## - (3) 	 Features: 6  	 mse.test.mean=32.7 	 Features: crim, rm, age, dis, ptratio, lstat 
## Generation 5:
## - (1) 	 Features: 10  	 mse.test.mean=31.9 	 Features: crim, indus, chas, rm, age, dis, rad, ptratio, b, lstat 
## - (2) 	 Features: 8  	 mse.test.mean=32.1 	 Features: crim, zn, chas, rm, dis, tax, ptratio, lstat 
## - (3) 	 Features: 7  	 mse.test.mean=31.1 	 Features: zn, rm, dis, rad, tax, ptratio, lstat 
## Generation 6:
## - (1) 	 Features: 10  	 mse.test.mean=31.1 	 Features: crim, zn, indus, rm, age, dis, tax, ptratio, b, lstat 
## Generation 7:
## - (1) 	 Features: 7  	 mse.test.mean=31.8 	 Features: crim, indus, rm, dis, rad, ptratio, lstat 
## - (2) 	 Features: 9  	 mse.test.mean=  31 	 Features: crim, zn, indus, rm, dis, tax, ptratio, b, lstat 
## - (3) 	 Features: 11  	 mse.test.mean=29.9 	 Features: crim, zn, indus, nox, rm, age, dis, tax, ptratio, b, ... 
## Generation 8:
## - (1) 	 Features: 9  	 mse.test.mean=31.3 	 Features: crim, indus, rm, age, dis, rad, ptratio, b, lstat 
## - (2) 	 Features: 9  	 mse.test.mean=  31 	 Features: crim, zn, indus, rm, dis, tax, ptratio, b, lstat 
## - (3) 	 Features: 10  	 mse.test.mean=31.1 	 Features: crim, zn, indus, rm, age, dis, tax, ptratio, b, lstat 
## Generation 9:
## - (1) 	 Features: 9  	 mse.test.mean=  31 	 Features: crim, zn, indus, rm, dis, tax, ptratio, b, lstat 
## - (2) 	 Features: 11  	 mse.test.mean=29.5 	 Features: crim, indus, nox, rm, age, dis, rad, tax, ptratio, b, ... 
## - (3) 	 Features: 10  	 mse.test.mean=30.7 	 Features: zn, indus, chas, rm, dis, rad, tax, ptratio, b, lstat 
## Generation 10:
## - (1) 	 Features: 9  	 mse.test.mean=  31 	 Features: crim, zn, indus, rm, dis, tax, ptratio, b, lstat 
## - (2) 	 Features: 11  	 mse.test.mean=30.2 	 Features: crim, zn, indus, chas, nox, rm, age, dis, tax, ptratio, ...
```


