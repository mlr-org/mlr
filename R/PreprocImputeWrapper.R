makeImputeWrapper = function(learner, ..., update.learner.chars=FALSE) {
  checkArg(learner, "Learner")
  args = list(...)
  if (!isProperlyNamed(args))
    stop("All arguments must be properly named")
  if ("data" %in% names(args))
    stop("You may not pass argument 'data' as parameter (handled internally)")
  if ("target" %in% names(args))
    stop("You may not pass argument 'target' as parameter (handled internally)")
  not.ok = names(args) %nin% names(formals(impute))
  if (any(not.ok))
    stopf("Function impute cannot handle arguments %s", collapse(names(args)[not.ok]))
  rm(not.ok)

  trainfun = function(data, target, args) {
    message("Imputing data ...")
    setNames(do.call(impute, c(list(data=data, target=target), args)),
      c("data", "control"))
  }

  predictfun = function(data, target, args, control) {
    message("Reimputing data ...")
    reimpute(data, control)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals=args)
  if (update.learner.chars)
    lrn$missings = TRUE
  lrn
}
