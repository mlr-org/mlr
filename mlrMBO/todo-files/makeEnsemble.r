makeRLEnsemble = function(id="RLEnsemble", base.learners) {
  checkArg(id, "character", len=1L, na.o=FALSE)
  checkArg(base.learners, "list")
  checkListElementClass(base.learners, "Learner")
  types = unique(extractSubList(base.learners, "type"))
  if (!identical(types, "regr"))
    stop("Base learners must all be regression learners!")
  pts = unique(extractSubList(base.learners, "predict.type"))
  
  lrn = structure(list(
      id = id,
      type = "regr",
      package = unique(extractSubList(base.learners, "package")),
      par.set = makeParamSet(),
      par.vals = list(),
      predict.type = "response",
      numerics = all(extractSubList(base.learners, "numerics")),
      factors = all(extractSubList(base.learners, "factors")),
      missings = all(extractSubList(base.learners, "missings")),
      weights = all(extractSubList(base.learners, "weights")),
      oneclass = FALSE,
      twoclass = FALSE,
      multiclass = FALSE,
      prob = FALSE,
      se = all(extractSubList(base.learners, "se"))
    ), class = c("RLEnsemble", "Learner"))
  lrn$base.learners = base.learners
  lrn$action.vals = matrix(NA, nrow=0, ncol=length(base.learners))
  lrn$sel.learner = integer(0)
  return(lrn)
}

trainLearner.RLEnsemble = function(.learner, .task, .subset,  ...) {
  bls = .learner$base.learners 
  sl = sample(1:length(bls), 1, prob=gibbs(action.vals[nrow(action.vals),]))
  sel.learners[j] = sl
  trainLearner(bls[[sl]], .task, .subset, ...)
}

