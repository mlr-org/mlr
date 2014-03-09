Multicriteria Evaluation
========================

For this example we have to deal with some of the *inner workings of mlr* and the characteristics of different *measures*.
In a lot of cases you get more than just one measure value but are interested in just one *aggregated value*.
For instance in a 10-fold cross validation you obtain 10 values for the *mmce*.
The *aggregated value* is the mean of these 10 values.
**mlr** knows how to handle it because each `measure` knows how it is aggregated:

```splus
library(mlr)
mmce$aggr  #Mean misclassification error
```

```
## Aggregation function: test.mean
```

```splus
rmse$aggr  #Root mean square error
```

```
## Aggregation function: test.sqrt.of.mean
```

You can also create a new `measure` by using a different aggregation (see `?aggregations`).

```splus
mmceAggrBySd = setAggregation(mmce, test.sd)

task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.rpart")
rdesc = makeResampleDesc("CV", iters = 5)
r = resample(lrn, task, rdesc, measures = list(mmce, mmceAggrBySd))
```

```
## Loading packages on master (to be available on slaves for mode local): mlr
## [Resample] cross-validation iter: 1
## [Resample] cross-validation iter: 2
## [Resample] cross-validation iter: 3
## [Resample] cross-validation iter: 4
## [Resample] cross-validation iter: 5
## [Resample] Result: mmce.test.mean=0.0667,mmce.test.sd=0.0333
```


It is even possible to create your own *aggregation funtion* by calling the intern **mlr** function `makeAggregation()`.
You can use intern (also *not exported*) functions of R-packages by `packagename:::function()`.
So in this case:

```splus
mlr:::makeAggregation(id = "some.name", fun = function(task, perf.test, perf.train, 
    measure, group, pred) {
    # stuff you want to do with perf.test or perf.train
})
```

```
## Aggregation function: some.name
```

Remember: It is important that the head of the function looks exactly like given above!
`perf.test` and `perf.train` are both numerical vectors containing the measure values.
In the usual cases (e.g. *cross validation*) the `perf.train` vector is empty.

Let's say you are interested in the range of the obtained measures:

```splus
my.range.aggr = mlr:::makeAggregation(id = "test.range", fun = function(task, 
    perf.test, perf.train, measure, group, pred) diff(range(perf.test)))
```


Now we can run a feature selection based on the first measure in the provided list and see how the other measures turn out.

```splus
library(ggplot2)
ms1 = mmce
ms2 = setAggregation(ms1, my.range.aggr)
ms1min = setAggregation(ms1, test.min)
ms1max = setAggregation(ms1, test.max)
(res = selectFeatures(lrn, task, rdesc, measures = list(ms1, ms2, ms1min, ms1max), 
    control = makeFeatSelControlExhaustive(), show.info = FALSE))
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
```

```
## FeatSel result:
## Features (2): Sepal.Length, Petal.Width
## mmce.test.mean=0.06,mmce.test.range= 0.1,mmce.test.min=0.0333,mmce.test.max=0.133
```

```splus
perf.data = as.data.frame(res$opt.path)
p = ggplot(aes(x = mmce.test.mean, y = mmce.test.range, xmax = mmce.test.max, 
    xmin = mmce.test.min, color = as.factor(Sepal.Width), pch = as.factor(Petal.Width)), 
    data = perf.data) + geom_point(size = 4) + geom_errorbarh(height = 0)
print(p)
```

![plot of chunk MulticriteriaEvaluation](figs/multicriteria_evaluation/MulticriteriaEvaluation.png) 

