Parallelization
===============

*R* usually doesn't make use of multiple cores.
With the integration of [parallelMap](https://github.com/berndbischl/parallelMap) into **mlr** it becomes easy to activate the parallel computing already implemented into **mlr**.


```splus
library("mlr")
library("parallelMap")
parallelStartSocket(2)
```

```
## Starting parallelization in mode=socket with cpus=2.
```

```splus
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.lda")
rdesc = makeResampleDesc("CV", iters = 4)
r = resample(lrn, task, rdesc)
```

```
## Mapping in parallel: mode=socket; cpus=2; elements=4.
## [Resample] Result: mmce.test.mean=0.0201
```

```splus
parallelStop()
```

```
## Stopped parallelization. All cleaned up.
```


Under Linux or OSX you might want to use `parallelStartMulticore()` instead.

For further ways of parallelizing the computing consult the [parallelMap tutorial](https://github.com/berndbischl/parallelMap#parallelmap) and help.
