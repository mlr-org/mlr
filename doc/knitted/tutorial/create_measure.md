


Integrating another measure
===========================

Maybe you want to evaluate a prediction with a measure, which ist not yet implemented 
in **mlr**. This can either be a performance measure, which is not included in the in mlr 
provided list of [measures](http://berndbischl.github.io/mlr/measures.html) or a measure using a non-standard misclassification cost matrix.


Construct performance measure
-----------------------------

The [makeMeasure](http://berndbischl.github.io/mlr/makeMeasure.html) function is a simple way to construct your own performance 
measure. In the following this is exemplified by an implemantion of the mean 
misclassification error (mmce) for the iris dataset. For this purpose we write a simple 
function, that computes the measure on the basis of the predictions and subsequently wrap it 
in a Measure object. Then, we work with it as usual with the [performance](http://berndbischl.github.io/mlr/performance.html) function. See the 
R documentation of the [makeMeasure](http://berndbischl.github.io/mlr/makeMeasure.html) function for details on the various parameters.


```r
# Define the measure.
my.mmce <- function(task, model, pred, extra.args) {
    tb <- table(pred$data$response, pred$data$truth)
    1 - sum(diag(tb))/sum(tb)
}

# Encapsulate the function with a Measure object.
my.mmce <- makeMeasure(id = "my.mmce", minimize = TRUE, classif = TRUE, allowed.pred.types = "response", 
    fun = my.mmce)

# Create classification task and learner
task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.lda")
mod <- train(lrn, task)
```

```
## Error: cannot coerce class "c("classif.lda", "RLearnerClassif",
## "RLearner", "Learner")" to a data.frame
```

```r
pred <- predict(mod, newdata = iris)
```

```
## Error: Fehler bei der Auswertung des Argumentes 'object' bei der Methodenauswahl
## für Funktion 'predict': Fehler: Objekt 'mod' nicht gefunden
```

```r

# Compare predicted and true label with our measure.
performance(pred, measures = my.mmce)
```

```
## Error: Objekt 'pred' nicht gefunden
```

```r

# Apparently the result coincides with the mlr implementaion.
performance(pred, measures = mmce)
```

```
## Error: Objekt 'pred' nicht gefunden
```



Construct measure for non-standard misclassification costs
----------------------------------------------------------

For creating a measure that involves non-standard misclassification costs you can use
the [makeCostMeasure](http://berndbischl.github.io/mlr/makeCostMeasure.html) function. In order to do this, you first need to define the cost
matrix you want to use and include all class labels. The cost matrix can then be 
wraped in a Measure object and then a prediction can be evaluated as normally with the
[performance](http://berndbischl.github.io/mlr/performance.html) function. See the R documentation of the [makeCostMeasure](http://berndbischl.github.io/mlr/makeCostMeasure.html) function for 
details on the various parameters.


```r
# Create misclassification cost matrix.
mcm <- matrix(c(0, 2, 2, 3, 0, 2, 1, 1, 0), ncol = 3, dimnames = list(c("setosa", 
    "versicolor", "virginica"), c("setosa", "versicolor", "virginica")))

# Create classification task and learner
task <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner("classif.lda")
mod <- train(lrn, task)
```

```
## Error: cannot coerce class "c("classif.lda", "RLearnerClassif",
## "RLearner", "Learner")" to a data.frame
```

```r
pred <- predict(mod, newdata = iris)
```

```
## Error: Fehler bei der Auswertung des Argumentes 'object' bei der Methodenauswahl
## für Funktion 'predict': Fehler: Objekt 'mod' nicht gefunden
```

```r

# Encapsulate the cost matrix in a Measure object.
my.costs <- makeCostMeasure(id = "costs", minimize = TRUE, costs = mcm, task, 
    aggregate = mean)

# Compare predicted and true label with our measure.
performance(pred, measures = my.costs)
```

```
## Error: Objekt 'pred' nicht gefunden
```



