# Style Guide

* Please write **R** when referring to R.
* Please write [%mlr] and [%package] referring to mlr or any other package. This will link to 
  rdocumentation.org.

## Headers

* Headers beginning with # are capitalized.
* Headers beginning with ##, ###, etc. are not capitalized.

## R code

* Comments in R code start with ## and the first word is capitalized. 
  No dot at the end except the comment is a proper sentence
* Adhere to the [style guide](https://github.com/tudo-r/PackagesInfo/wiki/R-Style-Guide),
  e.g., regarding tabs, indentation, spaces around operators and commas.
* Composite names are separated by a dot, for example cluster.task, filtered.task,
  bag.lrn (for bagged learner), etc.

Names of frequently used **R** objects

| Name | **R** object |
| ---- | ------------ |
| task | Task |
| tasks | list of Tasks |
| lrn | Learner |
| lrns | list of Learners |
| mod | WrappedModel |
| pred | Prediction |
| perf | output of performance |
| ms | list of Measures |
| rdesc | ResampleDesc |
| rin | ResampleInst |
| in.rdesc | ResampleDesc in inner resampling loop |
| out.rdesc | ResampleDesc in outer resampling loop |
| out.rin | ResampleInstance in outer resampling loop |
| r | ResamplePrediction |
| res | some results, for example of benchmark |
| fv | filter values |
| sfeats | selected features |
| ps | ParamSet |
| ctrl | control object |
| df | some data.frame |

