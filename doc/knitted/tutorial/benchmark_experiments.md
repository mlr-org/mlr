Benchmark Experiments
=====================


In order to get an unbiased estimate of the performance on new data,
it is generally not enough to simply use repeated cross-validations
for a given set of hyperparameters and methods (see section [tuning](tune.md)), 
as this might produce an overly optimistic result.

A better (although more time-consuming) approach is nesting two
resampling methods.  To make the explanation easier, let's take
cross-validations, in this case also called "double cross-validation".
In the so called "outer" cross-validation the data is split repeatedly
into a (larger) training set and a (smaller) test set in the usual
way. Now, in every outer iteration the learner is tuned on the
training set by performing an "inner" cross-validation. The best found
hyperparameters are selected and afterwards used for fitting the learner 
to the complete "outer" training set. The resulting model is used to
access the (outer) test set. This results in much more reliable
estimates of the true performance distribution of the learner for unseen
data. These can now be used to estimate locations (e.g. of the mean
or median performance value) and to compare learning methods in a fair
way.

In the following we will see four examples to show different benchmark settings:

* One data set  +	two classification algorithms
* One data set	+	one classification algorithm	+	tuning
* Three data sets	+	three classification algorithms	+	tuning
* One data set	+	two classification algorithms	+	variable selection


Example 1: One task, two learners, no tuning
----------------------------------------------


```splus
library("mlr")

## Classification task with iris data set
task = makeClassifTask(data = iris, target = "Species")

## Two learners to be compared
learners = c("classif.lda", "classif.qda")

## Define cross-validation indices
rdesc = makeResampleDescription("CV", iters = 5)

res <- bench.exp(learners, task, rdesc)
```


The above code should be mainly self-explanatory. In the result every
column corresponds to one learner.  The entries show the mean test
error and its standard deviation for the final fitted model.

But the Benchmark result contains much more information, which you can
access if you want to see details. Let's have a look to the benchmark
result from the example above:


```splus
## Access further information The single performances of the cross-validation
## runs
res["perf"]

## Confusion matrices - one for each learner
res["conf.mats"]
```

	
Example 2: One task, one learner, tuning
----------------------------------------

Now we have a learner with hyperparameters and we want to find out,
which are the best ones. In that case we have two resampling levels.

We show an example with outer bootstrap and inner cross-validation,
our learner will be k-nearest-neighbor.


```splus
## Classification task with iris data set
task = makeClassifTask(data = iris, target = "Species")

## Range of hyperparameter k
ps = makeParameterSet(makeDiscreteParameter("k", 1:5))
ctrl = makeTuneControlGrid(ranges = ps)

## Define 'inner' cross-validation indices
inner.rdesc = makeResampleDesc("CV", iters = 3)

## Tune k-nearest-neighbor
lrn = makeTuneWrapper("classif.kknn", resampling = inner.rdesc, control = ctrl)

## Define 'outer' bootstrap indices
rdesc = makeResampleDesc("BS", iters = 5)

## Merge it to a benchmark experiment Choose accuracy instead of default
## measure mean misclassification error
res = bench.exp(lrn, task, rdesc, measure = acc)

## Which performances did we get in the single runs?
res["perf"]

## Which parameter belong to the performances?
res["tuned.par"]

## What does the confusion matrix look like?
res["conf.mats"]
```


Of course everything works the same way if we exchange the resampling
strategy either in the outer or inner run.  They can be freely
mixed.

Example 3: Three tasks, three learners, tuning
----------------------------------------------

Extensive example which shows a benchmark experiment with three data
sets, three learners and tuning.


```splus
library("dprep")
library("mlbench")
data(BreastCancer)
data(Vehicle)

## Classification task with three data sets
task1 = makeClassifTask("Iris", data = iris, target = "Species")
task2 = makeClassifTask("Vehicle", data = Vehicle, target = "Class")
task3 = makeClassifTask("BreastCancer", data = na.omit(BreastCancer), target = "Class", 
    excluded = "Id")

## Merge to one task
tasks = list(task1, task2, task3)

## Very small grid for SVM hyperparameters
ps = makeParameterSet(makeDiscreteParameter("C", 2^seq(-1, 1)), makeDiscreteParameter("sigma", 
    2^seq(-1, 1)))
ctrl = makeTuneControlGrid(ranges = ps)

## Define 'inner' cross-validation indices
inner.rdesc = makeResampleDesc("CV", iters = 3)

## Tune a SVM
lrn = makeTuneWrapper("classif.ksvm", method = "grid", resampling = inner.rdesc, 
    control = ctrl)

## Three learners to be compared
learners = c("classif.lda", "classif.rpart", lrn)

## Define 'outer' cross-validation indices
rdesc = makeResampleDesc("CV", iters = 5)

## Merge it to a benchmark experiment
res = bench.exp(learners, tasks, rdesc)

## Only for one task
res["perf", task = "Iris"]

## Only for one learner
res["perf", learner = "classif.lda"]

## Tuned parameter for SVM
res["tuned.par", learner = "classif.ksvm"]

## Confusion matrix for one learner and one task
res["conf.mats", learner = "classif.rpart", task = "BreastCancer"]

## Optimal performance of the inner (!) resampling, i.e. here 3-fold
## cross-validation
res["opt.perf", learner = "classif.ksvm"]
```


Example 4: One task, two learners, variable selection
-----------------------------------------------------

Let's see how we can do [variable selection](variable_selection.md) in
a benchmark experiment:


```splus
## Classification task with iris data set
task = makeClassifTask("iris", data = iris, target = "Species")

## Control object for variable selection
ctrl = makeFeatselControlSequential(beta = 100, method = "sfs")

## Inner resampling
inner.rdesc = makeResampleDesc("CV", iter = 2)

## Variable selection with Sequential Forward Search
lrn = makeFeatselWrapper("classif.lda", resampling = inner.rdesc, control = ctrl)

## Let's compare two learners
learners = c("classif.rpart", lrn)

## Define outer resampling
rdesc = makeResampleDesc("subsample", iter = 3)

## Merge to a benchmark experiment
res = bench.exp(tasks = task, learners = learners, resampling = rdesc)

## Which variables have been selected (in the outer resampling steps)?
res["sel.var", learner = "classif.lda"]
```


<!--(
.. |benchmark_processing| image:: /_images/benchmark_processing.png
     :align: middle
     :width: 40em
     :alt: Benchmark processing pipeline
)-->
