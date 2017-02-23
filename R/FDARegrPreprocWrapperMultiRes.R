#' @title MultiRes preprocess wrapper
#'
#' @description Extracts multiResolution features and combine that with a
#'   learner.
#'
#' @param learner \cr
#'   The mlr learner to wrap on top of feature extraction.
#'
#' @export
makeFDARegrPreprocWrapperMultiRes = function(learner) {
  requirePackages("stringi", default.method = "load")
  lrn = checkLearner(learner)
  trainfun = function(data, target, args) {
    curve.lens = args$curve.lens
    res.level = args$res.level
    shift = args$shift
    A = extractFDAMultiResFeatures(data, curve.lens, res.level , shift)
    control = list(args)
    list(data = A, control = control) # Preprocessing train must return a list with elements data[data.frame] and control[list]!
  }

  predictfun = function(data, target, args, control) {
    return(trainfun(data, target, args)$data)
  }
  lrn2 = makePreprocWrapper(lrn, train = trainfun, predict = predictfun, par.vals = list())
  lrn2$id = stringi::stri_replace(lrn2$id, replacement = ".multiRes", regex = "\\.preproc$")
  lrn2 = addClasses(lrn2, "MultiResFeaturesWrapper")
  return(lrn2)
}
