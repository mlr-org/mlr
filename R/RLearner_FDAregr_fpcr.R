#' @title Functional principal component regression
#'
#' @description Functional learner for principal component regression.
#'
#' @export
makeRLearner.fdaregr.fpcr = function() {
  makeRLearnerRegr(
    cl = "fdaregr.fpcr",
    package = "refund",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ncomp", default = 200L),
      makeUntypedLearnerParam(id = "index.list", default = NULL)
    ),
    properties = c("numerics"),
    name = "FPCR regression",
    short.name = "fpcr"
  )
}

#' @export
trainLearner.fdaregr.fpcr = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, subset = .subset, target.extra = TRUE)
  tn = getTaskTargetNames(.task)
  #mextra_para  = list(...)
  #name4channel = names(index.list)
  #num4channel = length(index.list)
  #fit.af.s <- refund::pfr(pasat ~ af(cca, basistype="s", Qtransform=TRUE, k=50),
  #                data=DTI1)

  #fpcr(y = d$target, xfuncs = d$data, ncomp = mextra_para$ncomp)
}

#' @export
predictLearner.fdaregr.fpcr = function(.learner, .model, .newdata, ...) {

}
