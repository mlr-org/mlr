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
Different methods are built into **mlr**'s function `getFeatureFilterValues()` all accessing filter algorithms from the package `FSelector`.
The function is given a `task` and simply returns an importance vector.

```r
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
importance = getFeatureFilterValues(task, method = "information.gain")
```

```
## Loading required package: FSelector
```

```r
sort(importance, decreasing = TRUE)
```

```
##  Petal.Width Petal.Length Sepal.Length  Sepal.Width 
##       1.3784       1.3565       0.6523       0.3856
```

So according to this filter `Petal.Width` and `Petal.Length` contain the most *information*.
With **mlr**'s function `filterFeatures()` you can now filter the task by leaving out all
but a set number of features with the highest feature importance now into the task. 

```r
filtered.task = filterFeatures(task, feat.importance = importance, n = 2L)
```

Other filter options in `filterFeatures()' are to require a percentage of features filtered instead or to set a threshold for the numerical importance values.

In a proper experimental set up you might want to automate the selection of the variables so that it can be part of the validation method of your choice.
We will use the standard 10-fold cross validation.

```r
learner = makeLearner("classif.fnn")
learnerFiltered = makeFilterWrapper(learner = learner, fw.method = "information.gain", 
    fw.percentage = 0.7)
rdesc = makeResampleDesc("CV", iters = 10)
rsres = resample(learner = learnerFiltered, task = task, resampling = rdesc, 
    show.info = FALSE, models = TRUE)
rsres$aggr
```

```
## mmce.test.mean 
##        0.04667
```

Now you want might want to know which features have been used.
Luckily we have called `resample` with the argument `models=TRUE` which means that `rsres$models` contains a `list` of each model used for a fold.
In this case the `Learner` is also of the class `FilterWrapper` and we can call `getFilteredFeatures()` on each model.

```r
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


```r
library("mlr")
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.rpart")
rdesc = makeResampleDesc("Holdout")

ctrlSeq = makeFeatSelControlSequential(method = "sfs")
sfSeq = selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlSeq)
```

```
## [selectFeatures] 1: 0 bits: mmce.test.mean= 0.7
## [selectFeatures] 2: 1 bits: mmce.test.mean= 0.3
## [selectFeatures] 2: 1 bits: mmce.test.mean=0.56
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
## mmce.test.mean=0.04
```

```r
analyzeFeatSelResult(sfSeq, reduce = FALSE)
```

```
## Features         : 1
## Performance      : mmce.test.mean=0.04
## Petal.Length
## 
## Path to optimum:
## --------------------------------------------------------------------------------
## - Features:    0  Init   :                       Perf = 0.70  Diff:  0.00  *
## --------------------------------------------------------------------------------
## - Features:    1  Add    : Sepal.Length          Perf = 0.30  Diff: -0.26   
## - Features:    1  Add    : Sepal.Width           Perf = 0.56  Diff: -0.52   
## - Features:    1  Add    : Petal.Length          Perf = 0.04  Diff:  0.00  *
## - Features:    1  Add    : Petal.Width           Perf = 0.06  Diff: -0.02   
## --------------------------------------------------------------------------------
## - Features:    2  Add    : Sepal.Length          Perf = 0.04  Diff:  0.00   
## - Features:    2  Add    : Sepal.Width           Perf = 0.04  Diff:  0.00   
## - Features:    2  Add    : Petal.Width           Perf = 0.06  Diff: -0.02   
## 
## Stopped, because no improving feature was found.
```



#### Regression example

We fit a simple linear regression model to the ``BostonHousing`` data set and use a genetic algorithm to find a feature set that reduces the **mse** (mean squared error).


```r
library("mlbench")
data(BostonHousing)

task = makeRegrTask(data = BostonHousing, target = "medv")
lrn = makeLearner("regr.lm")
rdesc = makeResampleDesc("Holdout")

ctrlGA = makeFeatSelControlGA(maxit = 10)
sfGA = selectFeatures(learner = lrn, task = task, resampling = rdesc, control = ctrlGA, 
    show.info = FALSE)
sfGA
```

```
## FeatSel result:
## Features (11): zn, indus, chas, rm, age, dis, rad, tax, ptratio, b, lstat
## mse.test.mean=19.1
```


