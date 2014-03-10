Learning Tasks
==============

Learning tasks are the basic elements of the package to encapsulate the
data set and all relevant information regarding the purpose of the
task. This will be at least the target variable, but might also be
information about excluded (ID) variables or case weights.

Currently two subclasses of a [LearnTask](http://berndbischl.github.io/mlr/man/makeLearner.html) exist: [ClassifTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) for
classification and [RegrTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) for regression problems.  A
classification task is created by using the [makeClassifTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) factory
method. The target variable is converted to a ``factor``, if it is a
``logical``, ``integer`` or ``character`` vector. Accordingly use [makeRegrTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html)
in order to define a regression task. The target variable is converted 
to a ``numeric``.

Quick start
-----------

### Classification example

In the following example we define a classification task for the data
set ``BreastCancer`` (from the package mlbench) and exclude the ID
variable from all further model fitting and evaluation.


```splus
library("mlr")
library("mlbench")
data(BreastCancer)

df = BreastCancer
df$Id = NULL
task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")
```


Now, let us examine the created task:


```splus
task
```

```
## Supervised task: BreastCancer
## Type: classif
## Target: Class
## Observations: 699
## Features:
## numerics  factors 
##        0        4 
## Missings: TRUE
## Has weights: FALSE
## Has blocking: FALSE
## Classes: 2
##    benign malignant 
##       458       241 
## Positive class: benign
```


The now defined task contains basic information about the data,
e.g. the types of the features, the number of observations and the number
of missing values, as well as the classification problem, e.g. the
name of the target variable and the number of observations per class.


### Regression example

We will generally take the ``BostonHousing`` data set as regression example.


```splus
library("mlr")
library("mlbench")
data(BostonHousing)
df = BostonHousing
task = makeRegrTask(id = "BostonHousing", data = df, target = "medv")
task
```

```
## Supervised task: BostonHousing
## Type: regr
## Target: medv
## Observations: 506
## Features:
## numerics  factors 
##       12        1 
## Missings: FALSE
## Has weights: FALSE
## Has blocking: FALSE
```



Further information
-------------------

### Classification example

Let's have another look at the classification example.


```splus
library("mlr")
library("mlbench")
data(BreastCancer)

df = BreastCancer
df$Id = NULL
task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")
task
```

```
## Supervised task: BreastCancer
## Type: classif
## Target: Class
## Observations: 699
## Features:
## numerics  factors 
##        0        4 
## Missings: TRUE
## Has weights: FALSE
## Has blocking: FALSE
## Classes: 2
##    benign malignant 
##       458       241 
## Positive class: benign
```


As this is a binary problem, we see that a positive class is selected
by default. This will generally be the first class in the factor
levels of the target. You probably want to select this manually for
your applications.


```splus
task = makeClassifTask(id = "BreastCancer", data = df, target = "Class", positive = "malignant")
```


The [makeClassifTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) help page lists several other arguments that can be 
passed to describe further details of the classification problem.

There are some convenient methods to access properties of the data
set and the classification problem. Look at the documentation of the
[LearnTask](http://berndbischl.github.io/mlr/man/makeLearner.html) class and its subclasses [ClassifTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) and [RegrTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html), if
you are interested in the kind of "getter"-functions, which are
available. Here are some examples:

Get the names of the input variables.


```splus
getTaskFeatureNames(task)
```

```
## [1] "Cl.thickness"    "Cell.size"       "Cell.shape"      "Marg.adhesion"  
## [5] "Epith.c.size"    "Bare.nuclei"     "Bl.cromatin"     "Normal.nucleoli"
## [9] "Mitoses"
```


Get values of the target variable for all observations.


```splus
head(getTaskTargets(task))
```

```
## [1] benign    benign    benign    benign    benign    malignant
## Levels: benign malignant
```


All information can be obtained from the slots of the [LearnTask](http://berndbischl.github.io/mlr/man/makeLearner.html)
object directly.

The main part of the information is stored in the slot called `desc`,
which stands for description. (`task$task.desc` is an object of class [TaskDesc](http://berndbischl.github.io/mlr/man/TaskDesc.html).)


```splus
task$task.desc
```

```
## $id
## [1] "BreastCancer"
## 
## $type
## [1] "classif"
## 
## $target
## [1] "Class"
## 
## $size
## [1] 699
## 
## $n.feat
## numerics  factors 
##        0        4 
## 
## $has.missings
## [1] TRUE
## 
## $has.weights
## [1] FALSE
## 
## $has.blocking
## [1] FALSE
## 
## $class.levels
## [1] "benign"    "malignant"
## 
## $positive
## [1] "malignant"
## 
## $negative
## [1] "benign"
## 
## attr(,"class")
## [1] "ClassifTaskDesc" "TaskDesc"
```

```splus
str(task$task.desc)
```

```
## List of 11
##  $ id          : chr "BreastCancer"
##  $ type        : chr "classif"
##  $ target      : chr "Class"
##  $ size        : int 699
##  $ n.feat      : Named int [1:2] 0 4
##   ..- attr(*, "names")= chr [1:2] "numerics" "factors"
##  $ has.missings: logi TRUE
##  $ has.weights : logi FALSE
##  $ has.blocking: logi FALSE
##  $ class.levels: chr [1:2] "benign" "malignant"
##  $ positive    : chr "malignant"
##  $ negative    : chr "benign"
##  - attr(*, "class")= chr [1:2] "ClassifTaskDesc" "TaskDesc"
```


Optionally, we can include further information like a blocking factor 
into the task. Via the blocking argument you can specify if some 
observations "belong together". Specifically, they are either put all
in the training or the test set during a resampling iteration. The
blocking argument is a factor of the same length as the number of
observations in the data set, where observations with the same factor
level belong to the same block.

Now, let's include a (nonsensical) blocking structure:


```splus
blocking = factor(rep(1:3, nrow(BreastCancer)/3))
task = makeClassifTask(id = "BreastCancer", data = df, target = "Class", blocking = blocking)
head(task$blocking)
```

```
## [1] 1 2 3 1 2 3
## Levels: 1 2 3
```

```splus
table(task$blocking)
```

```
## 
##   1   2   3 
## 233 233 233
```


From this classification task we can now train various models, which
will be covered in the section [Training](train.md).
Before that, let's look at the regression experiment again.


### Regression example


```splus
library("mlr")
library("mlbench")
data(BostonHousing)
task = makeRegrTask(data = BostonHousing, target = "medv")
task
```

```
## Supervised task: data
## Type: regr
## Target: medv
## Observations: 506
## Features:
## numerics  factors 
##       12        1 
## Missings: FALSE
## Has weights: FALSE
## Has blocking: FALSE
```


The "getter" functions work analogous to the classification example.


```splus
getTaskFeatureNames(task)
```

```
##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
##  [8] "dis"     "rad"     "tax"     "ptratio" "b"       "lstat"
```

```splus

head(getTaskTargets(task))
```

```
## [1] 24.0 21.6 34.7 33.4 36.2 28.7
```


Inspect [TaskDesc](http://berndbischl.github.io/mlr/man/TaskDesc.html).


```splus
task$task.desc
```

```
## $id
## [1] "data"
## 
## $type
## [1] "regr"
## 
## $target
## [1] "medv"
## 
## $size
## [1] 506
## 
## $n.feat
## numerics  factors 
##       12        1 
## 
## $has.missings
## [1] FALSE
## 
## $has.weights
## [1] FALSE
## 
## $has.blocking
## [1] FALSE
## 
## $class.levels
## [1] NA
## 
## $positive
## [1] NA
## 
## $negative
## [1] NA
## 
## attr(,"class")
## [1] "RegrTaskDesc" "TaskDesc"
```

```splus
str(task$task.desc)
```

```
## List of 11
##  $ id          : chr "data"
##  $ type        : chr "regr"
##  $ target      : chr "medv"
##  $ size        : int 506
##  $ n.feat      : Named int [1:2] 12 1
##   ..- attr(*, "names")= chr [1:2] "numerics" "factors"
##  $ has.missings: logi FALSE
##  $ has.weights : logi FALSE
##  $ has.blocking: logi FALSE
##  $ class.levels: chr NA
##  $ positive    : chr NA
##  $ negative    : chr NA
##  - attr(*, "class")= chr [1:2] "RegrTaskDesc" "TaskDesc"
```


