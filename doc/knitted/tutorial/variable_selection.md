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
## Loading packages on master (to be available on slaves for mode local): mlr
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
The `Sepal.Width` did not make it into a single fold.

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
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 1: 0 bits: mmce.test.mean=0.62
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.34
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.52
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.06
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.06
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.06
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.06
## Loading packages on master (to be available on slaves for mode local): mlr
## [selectFeatures] 3: 2 bits: mmce.test.mean=0.06
```

```splus
sfSeq
```

```
## FeatSel result:
## Features (1): Petal.Length
## mmce.test.mean=0.06
```

```splus
analyzeFeatSelResult(sfSeq, reduce = FALSE)
```

```
## FeatSel result:
## - features (1): Petal.Length
## - Performance: mmce.test.mean=0.06
## 
## Path to optimum:
## - Features: 0  	 Initial model 	 Gain: mmce.test.mean=   0 	 SELECTED
## Finished step: 1 with 	 mmce.test.mean=0.62 	 Optimum: FALSE
## - Features: 1  	 Added: Sepal.Length 	 Gain: mmce.test.mean=-0.28 	 
## - Features: 1  	 Added: Sepal.Width 	 Gain: mmce.test.mean=-0.1 	 
## - Features: 1  	 Added: Petal.Length 	 Gain: mmce.test.mean=-0.56 	 
## - Features: 1  	 Added: Petal.Width 	 Gain: mmce.test.mean=-0.56 	 SELECTED
## Finished step: 2 with 	 mmce.test.mean=0.06 	 Optimum: TRUE
## - Features: 2  	 Added: Sepal.Length 	 Gain: mmce.test.mean=   0 	 
## - Features: 2  	 Added: Sepal.Width 	 Gain: mmce.test.mean=   0 	 
## - Features: 2  	 Added: Petal.Length 	 Gain: mmce.test.mean=   0 	 
## Finished step: 3 with 	 mmce.test.mean=0.06 	 Optimum: TRUE
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
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
## Loading packages on master (to be available on slaves for mode local): mlr
```

```splus
sfGA
```

```
## FeatSel result:
## Features (12): crim, zn, indus, nox, rm, age, dis, rad, tax, ptratio, b, lstat
## mse.test.mean=  23
```

```splus
analyzeFeatSelResult(sfGA)
```

```
## FeatSel result:
## - features (12): crim, zn, indus, nox, rm, age, dis, rad, tax, ptratio, ...
## - Performance: mse.test.mean=  23
## 
## Path to optimum:
## Initial generation:- (1) 	 Features: 11  	 mse.test.mean=27.1 	 Features: crim, zn, chas, nox, rm, age, rad, tax, ptratio, b, ... 
## - (2) 	 Features: 8  	 mse.test.mean=28.2 	 Features: zn, indus, rm, age, tax, ptratio, b, lstat 
## - (3) 	 Features: 6  	 mse.test.mean=55.2 	 Features: crim, indus, nox, dis, rad, tax 
## - (4) 	 Features: 4  	 mse.test.mean=43.1 	 Features: zn, indus, rm, age 
## - (5) 	 Features: 6  	 mse.test.mean=  51 	 Features: zn, chas, age, dis, tax, b 
## - (6) 	 Features: 5  	 mse.test.mean=56.3 	 Features: chas, age, rad, ptratio, b 
## - (7) 	 Features: 6  	 mse.test.mean=37.6 	 Features: zn, rm, age, tax, ptratio, b 
## - (8) 	 Features: 6  	 mse.test.mean=56.1 	 Features: crim, zn, chas, age, rad, ptratio 
## - (9) 	 Features: 4  	 mse.test.mean=62.5 	 Features: crim, nox, dis, tax 
## - (10) 	 Features: 2  	 mse.test.mean=62.8 	 Features: crim, ptratio 
## Generation 1:- (1) 	 Features: 5  	 mse.test.mean=32.4 	 Features: crim, nox, dis, tax, lstat 
## - (2) 	 Features: 6  	 mse.test.mean=42.3 	 Features: zn, chas, rm, age, rad, b 
## - (3) 	 Features: 8  	 mse.test.mean=26.4 	 Features: zn, indus, rm, age, dis, tax, b, lstat 
## Generation 2:- (1) 	 Features: 7  	 mse.test.mean=28.6 	 Features: zn, indus, chas, age, dis, tax, lstat 
## - (2) 	 Features: 10  	 mse.test.mean=27.7 	 Features: crim, indus, chas, rm, age, rad, tax, ptratio, b, lstat 
## - (3) 	 Features: 9  	 mse.test.mean=29.8 	 Features: crim, indus, chas, rm, age, dis, tax, b, lstat 
## Generation 3:- (1) 	 Features: 8  	 mse.test.mean=27.6 	 Features: crim, zn, indus, rm, age, dis, tax, lstat 
## - (2) 	 Features: 11  	 mse.test.mean=26.2 	 Features: crim, indus, chas, rm, age, dis, rad, tax, ptratio, b, ... 
## - (3) 	 Features: 4  	 mse.test.mean=30.1 	 Features: dis, tax, ptratio, lstat 
## Generation 4:- (1) 	 Features: 7  	 mse.test.mean=  28 	 Features: indus, rm, age, rad, tax, ptratio, lstat 
## - (2) 	 Features: 6  	 mse.test.mean=28.5 	 Features: crim, zn, nox, dis, tax, lstat 
## - (3) 	 Features: 11  	 mse.test.mean=26.3 	 Features: crim, zn, indus, chas, rm, age, dis, rad, tax, ptratio, ... 
## Generation 5:- (1) 	 Features: 9  	 mse.test.mean=25.7 	 Features: indus, chas, nox, rm, dis, tax, ptratio, b, lstat 
## - (2) 	 Features: 8  	 mse.test.mean=26.6 	 Features: indus, rm, age, rad, tax, ptratio, b, lstat 
## - (3) 	 Features: 7  	 mse.test.mean=27.8 	 Features: crim, zn, indus, age, dis, tax, lstat 
## Generation 6:- (1) 	 Features: 9  	 mse.test.mean=26.3 	 Features: indus, nox, rm, age, rad, tax, ptratio, b, lstat 
## - (2) 	 Features: 10  	 mse.test.mean=25.8 	 Features: crim, zn, indus, rm, age, dis, tax, ptratio, b, lstat 
## - (3) 	 Features: 9  	 mse.test.mean=25.7 	 Features: indus, chas, nox, rm, dis, tax, ptratio, b, lstat 
## Generation 7:- (1) 	 Features: 11  	 mse.test.mean=26.2 	 Features: crim, indus, chas, rm, age, dis, rad, tax, ptratio, b, ... 
## - (2) 	 Features: 9  	 mse.test.mean=26.3 	 Features: indus, nox, rm, age, rad, tax, ptratio, b, lstat 
## - (3) 	 Features: 9  	 mse.test.mean=25.3 	 Features: zn, indus, rm, age, dis, tax, ptratio, b, lstat 
## Generation 8:- (1) 	 Features: 11  	 mse.test.mean=  24 	 Features: crim, zn, indus, rm, age, dis, rad, tax, ptratio, b, ... 
## - (2) 	 Features: 10  	 mse.test.mean=26.3 	 Features: zn, indus, chas, rm, age, dis, tax, ptratio, b, lstat 
## - (3) 	 Features: 8  	 mse.test.mean=25.3 	 Features: zn, rm, age, dis, tax, ptratio, b, lstat 
## Generation 9:- (1) 	 Features: 9  	 mse.test.mean=25.9 	 Features: crim, zn, rm, age, dis, tax, ptratio, b, lstat 
## - (2) 	 Features: 12  	 mse.test.mean=  23 	 Features: crim, zn, indus, nox, rm, age, dis, rad, tax, ptratio, ... 
## Generation 10:- (1) 	 Features: 12  	 mse.test.mean=  23 	 Features: crim, zn, indus, nox, rm, age, dis, rad, tax, ptratio, ... 
## - (2) 	 Features: 11  	 mse.test.mean=25.3 	 Features: zn, indus, chas, nox, rm, age, dis, tax, ptratio, b, ...
```


