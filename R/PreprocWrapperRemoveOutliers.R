#FIXME: read this, better way to require package
#only export if proved useful
# simul study?

makePreprocWrapperRemoveOutliers = function(learner, ro.alpha=0.5) {
  checkArg(learner, "Learner")

  trainfun = function(data, target, args) {
    require(robustbase)
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    # we must have at least n = 2*p obs
    if (length(nums) > 0 && nrow(data) >= 2*length(nums)) {
      x = data[, nums]
      # split x in classes
      x.splitted = split(x, data[,target])
      idx = lapply(x.splitted, function(d) as.logical(covMcd(x = d, alpha = args$ro.alpha)$mcd.wt))
      idx = unsplit(idx, data[,target])
      data = data[idx,]
      list(data=data, control=list())
    }
    list(data=data, control=list())
  }

  predictfun = function(data, target, args, control) {
    data
  }
  ps = makeParamSet(makeNumericLearnerParam("ro.alpha", lower=0, upper=1))
  pv = list(ro.alpha=ro.alpha)
  makePreprocWrapper(learner, trainfun, predictfun, par.set=ps, par.vals=pv)
}  






