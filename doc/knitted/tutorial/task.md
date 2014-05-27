Learning Tasks
==============

Learning tasks are the basic elements of the package to encapsulate the
data set and all relevant information regarding the purpose of the
task, e.g, the target variable.

Currently two subclasses of a [SupervisedTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) exist: [ClassifTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) for
classification and [RegrTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) for regression problems.
We are also working on survival analysis and a general definition of cost-sensitive learning.
A classification task is created by using [makeClassifTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html),
[makeRegrTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) is used for regression task.

In the following example, we define a classification task for the data
set ``BreastCancer`` (from the package mlbench) and exclude the ID
variable from all further model fitting and evaluation.


```splus
library("mlr")
library("mlbench")
data(BreastCancer)

df = BreastCancer
df$Id = NULL
classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")
classif.task
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


As we can see, the task records basic information about the data set,
e.g., the types of the features, the number of observations, whether
missing values are present, the number of observations per class and so on.


In many of the following regression examples we will use the ``BostonHousing`` data set:


```splus
data(BostonHousing)
regr.task = makeRegrTask(id = "BostonHousing", data = BostonHousing, target = "medv")
regr.task
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

Let's have another look at the classification example.

As this is a binary problem, we see that a positive class is selected
by default. This will generally be auto-selected, but you might
want to do this manually for your application. It mainly concerns ROC analysis, where in order
to talk about something like a true positive rate, we need to know which of the two classes is the positive
one.


```splus
classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class", 
    positive = "malignant")
```


There are also some convenient methods to access properties and parts of the task.
The most important ones are listed in [SupervisedTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html).

Here are some examples.



```splus
# get the names of the input variables:
getTaskFeatureNames(classif.task)
```

```
## [1] "Cl.thickness"    "Cell.size"       "Cell.shape"      "Marg.adhesion"  
## [5] "Epith.c.size"    "Bare.nuclei"     "Bl.cromatin"     "Normal.nucleoli"
## [9] "Mitoses"
```

```splus

# get values of the target variable for all observations:
head(getTaskTargets(classif.task))
```

```
## [1] benign    benign    benign    benign    benign    malignant
## Levels: benign malignant
```

```splus
head(getTaskTargets(regr.task))
```

```
## [1] 24.0 21.6 34.7 33.4 36.2 28.7
```

```splus

# accessing the data set:
str(getTaskData(classif.task))
```

```
## 'data.frame':	699 obs. of  10 variables:
##  $ Cl.thickness   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
##  $ Cell.size      : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
##  $ Cell.shape     : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
##  $ Marg.adhesion  : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
##  $ Epith.c.size   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
##  $ Bare.nuclei    : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
##  $ Bl.cromatin    : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
##  $ Normal.nucleoli: Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
##  $ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
##  $ Class          : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...
```


Note the many options [getTaskData](http://berndbischl.github.io/mlr/man/getTaskData.html) provides to convert the data set into a covenient format.
This is especially handy when you integrate a learner from another package into mlr.

If you are more technically inclined, you could also directly access
the information stored in the slot called `task$task.desc`,
which stands for "description" and is of class [TaskDesc](http://berndbischl.github.io/mlr/man/TaskDesc.html).


```splus
str(classif.task$task.desc)
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
##  - attr(*, "class")= chr [1:2] "TaskDescClassif" "TaskDesc"
```


The [SupervisedTask](http://berndbischl.github.io/mlr/man/SupervisedTask.html) help page also lists several other arguments
to describe further details of the problem.

E.g., we could include a blocking factor into the task.
This would tell the task that some observations "belong together", so they are either put all
in the training or the test set during a resampling iteration.
Or you could weight observations according to their importance.

