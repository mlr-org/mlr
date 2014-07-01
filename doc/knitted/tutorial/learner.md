Learners
========

These classes provide a unified interface to all
classification and regression methods in R. Some are already integrated,
others are not, but the package is specifically designed to make
extensions simple. If one of your favorite methods is missing, either
send the **mlr** maintainers an email or see the part of the tutorial
on [how to extend the package yourself](create_learner.md).

To see which learners are already implemented, have a look at the [learners](http://berndbischl.github.io/mlr/man/learners.html) documentation page.

A learner in **mlr** is constructed by calling [makeLearner](http://berndbischl.github.io/mlr/man/makeLearner.html).
In the constructor you have the following options:

* Setting hyperparameters.
* Setting the ID to name the object (some methods will later use this ID to name results).
* Controlling the output for later prediction, e.g., for classification,
  whether you want a factor for predicted classes or numerical probabilities.


```splus
# classifier, set it up for prediction probabilities
lrn = makeLearner("classif.rpart", predict.type = "prob")
# regression
lrn = makeLearner("regr.rpart")
# gradient boosting machine with hyperparameters and custon name
lrn = makeLearner("regr.gbm", id = "mygbm", n.trees = 500, interaction.depth = 3)
```


The first argument defines the underlying algorithm from R.
The naming conventions are ``classif.<R_method_name>`` for
classification methods and ``regr.<R_method_name>`` for regression methods.
The names of all learning methods are listed on the [learners](http://berndbischl.github.io/mlr/man/learners.html) help page.
The tables in [IntegratedLearners](../integrated_learners.md) provide a survey about these properties for all integrated learning methods.

Further information
-------------------

The generated learner is an object of class [Learner](http://berndbischl.github.io/mlr/man/makeLearner.html).
It contains the properties the method, e.g, which features it can handle,
what can be output during prediction, whether multi-class problems,
observations weights or missing values are supported and so on.


```splus
# classifier, set it up for prediction probabilities
lrn = makeLearner("classif.rpart", predict.type = "prob", minsplit = 20)
lrn
```

```
## Learner classif.rpart from package rpart
## Type: classif
## Class: classif.rpart
## Properties: twoclass,multiclass,missings,numerics,factors,prob,weights
## Predict-Type: prob
## Hyperparameters: xval=0,minsplit=20
```

```splus
# display stored information for hyperparameters
lrn$par.set
```

```
##                    Type len  Def   Constr Req Trafo
## minsplit        integer   -   20 1 to Inf   -     -
## minbucket       integer   -    - 1 to Inf   -     -
## cp              numeric   - 0.01   0 to 1   -     -
## maxcompete      integer   -    4 0 to Inf   -     -
## maxsurrogate    integer   -    5 0 to Inf   -     -
## usesurrogate   discrete   -    2    0,1,2   -     -
## surrogatestyle discrete   -    0      0,1   -     -
## maxdepth        integer   -   30  1 to 30   -     -
## xval            integer   -   10 0 to Inf   -     -
## parms           untyped   -    -        -   -     -
```


