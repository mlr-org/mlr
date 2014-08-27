#FIXME: read this, better way to require package
#only export if proved useful
# simul study?
# FIXME: indexing without drop

makePreprocWrapperRemoveOutliers = function(learner, ro.alpha = 0.5) {
  assertClass(learner, classes = "Learner")

  trainfun = function(data, target, args) {
    require(robustbase)
    cns = colnames(data)
    nums = setdiff(cns[vlapply(data, is.numeric)], target)
    # we must have at least n = 2*p obs
    if (length(nums) && nrow(data) >= 2L*length(nums)) {
      x = data[, nums, drop = FALSE]
      # split x in classes
      x.splitted = split(x, data[, target])
      idx = lapply(x.splitted, function(d) as.logical(covMcd(x = d, alpha = args$ro.alpha)$mcd.wt))
      idx = unsplit(idx, data[, target])
      data = data[idx,]
    }
    list(data = data, control = list())
  }

  predictfun = function(data, target, args, control) {
    data
  }

  makePreprocWrapper(
    learner,
    trainfun,
    predictfun,
    par.set = makeParamSet(makeNumericLearnerParam("ro.alpha", lower = 0, upper = 1)),
    par.vals = list(ro.alpha = ro.alpha)
  )
}
