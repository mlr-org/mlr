```{splus echo=FALSE}
## Preload mlr for all coming blocks
library("mlr")
``` 

Integrating another measure
===========================

Maybe you want to evaluate a prediction with a measure, which ist not yet implemented 
in **mlr**. This can either be a performance measure, which is not included in the in mlr 
provided list of [measures](http://berndbischl.github.io/mlr/man/measures.html) or a measure using a non-standard misclassification cost matrix.


Construct performance measure
-----------------------------

The [makeMeasure](http://berndbischl.github.io/mlr/man/makeMeasure.html) function is a simple way to construct your own performance 
measure. In the following this is exemplified by an implemantion of the mean 
misclassification error (mmce) for the iris dataset. For this purpose we write a simple 
function, that computes the measure on the basis of the predictions and subsequently wrap it 
in a Measure object. Then, we work with it as usual with the [performance](http://berndbischl.github.io/mlr/man/performance.html) function. See the 
R documentation of the [makeMeasure](http://berndbischl.github.io/mlr/man/makeMeasure.html) function for details on the various parameters.

```{splus}
# Define the measure.
my.mmce = function(task, model, pred, extra.args) {
  tb = table(pred$data$response, pred$data$truth)
  1 - sum(diag(tb)) / sum(tb)
}

# Encapsulate the function with a Measure object.
my.mmce = makeMeasure(id = "my.mmce", minimize = TRUE, classif = TRUE, 
                       allowed.pred.types = "response", fun = my.mmce)

# Create classification task and learner
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
mod = train(lrn, task)
pred = predict(mod, newdata= iris)

# Compare predicted and true label with our measure.
performance(pred, measures = my.mmce)

# Apparently the result coincides with the mlr implementaion.
performance(pred, measures = mmce)
```


Construct measure for non-standard misclassification costs
----------------------------------------------------------

For creating a measure that involves non-standard misclassification costs you can use
the [makeCostMeasure](http://berndbischl.github.io/mlr/man/makeCostMeasure.html) function. In order to do this, you first need to define the cost
matrix you want to use and include all class labels. The cost matrix can then be 
wraped in a Measure object and then a prediction can be evaluated as normally with the
[performance](http://berndbischl.github.io/mlr/man/performance.html) function. See the R documentation of the [makeCostMeasure](http://berndbischl.github.io/mlr/man/makeCostMeasure.html) function for 
details on the various parameters.

```{splus}
# Create misclassification cost matrix.
mcm = matrix(c(0, 2, 2, 3, 0, 2, 1, 1, 0), ncol=3, 
              dimnames = list(c("setosa", "versicolor", "virginica"),
                              c("setosa", "versicolor", "virginica")))
          
# Create classification task and learner
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
mod = train(lrn, task)
pred = predict(mod, newdata= iris)
          
# Encapsulate the cost matrix in a Measure object.                             
my.costs = makeCostMeasure(id = "costs", minimize = TRUE, costs=mcm, task, aggregate = mean)

# Compare predicted and true label with our measure.
performance(pred, measures = my.costs)
```


