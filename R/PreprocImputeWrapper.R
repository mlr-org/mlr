# FIXME this is untested :(
makeImputeWrapper = function(learner, ...) {
  checkArg(learner, "Learner")
  args = list(...)

  trainfun = function(data, target, ...) {
    setNames(impute(data=data, target=target, ...), c("data", "control"))
  }

  predictfun = function(data, target, args, control) {
    reimpute(data, control)
  }

  makePreprocWrapper(learner, trainfun, predictfun, par.vals=list(args=args))
}
