Over- and Undersampling
=======================

In the case you have uneven distributed classes in your training data but know that this won't be the case for the test data then it might be handy to oversample the minority or undersample the majority class.

data.frames
-----------

First we show how it is done for `data.frames`:


```splus
library(mlr)
classA = data.frame(x = rnorm(20, mean = 1), class = "A")
classB = data.frame(x = rnorm(80, mean = 2), class = "B")
data = rbind(classA, classB)
table(data$class)
```

```
## 
##  A  B 
## 20 80
```

```splus
data.over = oversample(data, "class", rate = 4)
```

```
## Error: unbenutztes Argument ("class")
```

```splus
table(data.over$class)
```

```
## Error: Objekt 'data.over' nicht gefunden
```

```splus
data.under = undersample(data, "class", rate = 1/4)
```

```
## Error: unbenutztes Argument ("class")
```

```splus
table(data.under$class)
```

```
## Error: Objekt 'data.under' nicht gefunden
```


tasks
-----

Now let's see what the effect is for a **classification task**:

```splus
data.even = rbind(data.frame(x = rnorm(50, mean = 1), class = "A"), data.frame(x = rnorm(50, 
    mean = 2), class = "B"))
task = makeClassifTask(data = data, target = "class")
task.over = oversample(task, rate = 4)
task.under = undersample(task, rate = 1/4)
lrn = makeLearner("classif.PART")
mod = train(lrn, task)
mod.over = train(lrn, task.over)
mod.under = train(lrn, task.under)
performance(predict(mod, newdata = data.even), measures = mmce)
```

```
## mmce 
##  0.5
```

```splus
performance(predict(mod.over, newdata = data.even), measures = mmce)
```

```
## mmce 
## 0.38
```

```splus
performance(predict(mod.under, newdata = data.even), measures = mmce)
```

```
## mmce 
## 0.38
```


learners
--------

Note that it is _highly dependent_ on the learner if _over_ or _undersampling_ is useful.
You are also able to wrap the over and undersampling within the _learner_.

```splus
lrn.over = makeOversampleWrapper(lrn, osw.rate = 4)
lrn.under = makeUndersampleWrapper(lrn, usw.rate = 1/4)
mod = train(lrn, task)
mod.over = train(lrn.over, task)
mod.under = train(lrn.under, task)
performance(predict(mod, newdata = data.even), measures = mmce)
```

```
## mmce 
##  0.5
```

```splus
performance(predict(mod.over, newdata = data.even), measures = mmce)
```

```
## mmce 
## 0.27
```

```splus
performance(predict(mod.under, newdata = data.even), measures = mmce)
```

```
## mmce 
## 0.33
```

