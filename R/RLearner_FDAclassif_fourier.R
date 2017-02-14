#' @title Fast Fourier Transform Learner
#'
#' @description Learner for the fast fourier transformation.
#'
#' @export
makeRLearner.fdaclassif.fourier = function() {
  makeRLearnerClassif(
    cl = "fdaclassif.fourier",
    package = "stats",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "include.target", default = FALSE),
      makeDiscreteLearnerParam(id = "fft.coeff", default = "phase", values = list("phase", "amplitude"))
    ),
    properties = c("twoclass", "multiclass", "numerics"),
    name = "Fourier transformation",
    short.name = "fourier"
  )
}

#' @export
trainLearner.fdaclassif.fourier = function(.learner, .task, .subset, .weights = NULL, ...) {

  z = getTaskData(.task, subset = .subset, target.extra = TRUE, recode.target = "-1+1")
  getFDAFourierFeatures(data = z$data, target = .task$task.desc$target, ...)
}

#' @export
predictLearner.tsclassif.fourier = function(.learner, .model, .newdata, ...) {

  #FIXME: prediction needs a "standard" learner.


}


