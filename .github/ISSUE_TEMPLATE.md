For bug reports, please respect the following guidelines and check the boxes accordingly.

For questions of the type: "How can this be done?", please consider posting them on Stackoverflow.

For everything else ignore this template.

## Bug report

- [ ] Start a new R session
- [ ] Install the latest version of mlr: `update.packages(oldPkgs="mlr", ask=FALSE)` or if you use a GitHub install of mlr: `devtools::install_github(c("BBmisc", "ParamHelpers", "mlr"))`
- [ ] Give a minimal reproducible example
- [ ] run `sessionInfo()`

### Minimal reproducible example:

```r
lrn = makeLearner("classif.lda")
resample(learner = lrn, task = iris.task, resampling = cv10)
```
