# Style Guide

* Please write **mlr** and **R** when referring to the mlr package and R.

## Headers

* Headers beginning with # are capitalized.
* Headers beginning with ##, ###, etc. are not capitalized.

## R code

* Comments in R code start with ## and the first word is capitalized. 
  No dot at the end except the comment is a proper sentence
* Use blanks around binary operators (=, -, *, etc.) and after commas.
* Names of frequently used R objects

task | Task
tasks | list of Tasks
lrn | Learner
lrns | list of Learners
mod | WrappedModel
pred | Prediction
perf | output of performance
ms | list of Measures
rdesc | ResampleDesc
rin | ResampleInst
in.rdesc | ResampleDesc in inner resampling loop
out.rdesc | ResampleDesc in outer resampling loop
out.rin | ResampleInstance in outer resampling loop
r | ResamplePrediction
res | some results, for example of benchmark
fv | filter values
sfeats | selected features
ps | ParamSet
ctrl | control object
df | some data.frame

* Composite names are separated by a dot, for example cluster.task, filtered.task, 
bag.lrn (for bagged learner), etc.
