#' @title MultiRes preprocess wrapper.
#'
#' @description
#' Extracts multiResolution features and combine that with a learner.
#'
#' @param learner \cr
#'   The mlr learner to wrap on top of feature extraction.
#' @param mrw.res.level [\code{integer(1)}]\cr
#'   FIX ME. Description.
#' @param mrw.shift [\code{numeric(1)}]\cr
#'   FIX ME. Description.
#' @return Object of type \code{learner}. FIX ME. What is returned?
#' @export
makeFDARegrPreprocWrapperMultiRes = function(learner, mrw.res.level = 3L, mrw.shift = 0.5) {
  lrn = checkLearner(learner)
  mrw.res.level = asInt(mrw.res.level, lower = 1L)
  assertNumber(mrw.shift, lower = 0, upper = 1)

  par.set = makeParamSet(
    makeIntegerParam("mrw.res.level", lower = 1),
    makeNumericParam("mrw.shift", lower = 0, upper = 1)
  )
  par.vals = list(mrw.res.level = mrw.res.level, mrw.shift = mrw.shift)

  # FIXME: this only works for numerical attributes in the curves! we need a check here?
  # FIXME: what happens with the scalar vars in feature extractions? we just copy them right?

  # FIXME: the whole thing DOES NOT WORK. we need "task" in the signature of trainfun. see issue #1520

  trainfun = function(data, target, args) {
    # cns.fd =
    # cns.scalar = setdiff(colnames(data))
    # d.fd = data
    # d.scalar
    # curve.lens =
    # d = extractFDAMultiResFeatures(d1, args$curve.lens, args$mrw.res.level , args$mrw.shift)
    # list(data = d, control = NULL)
  }

  predictfun = function(data, target, args, control) {
    return(trainfun(data, target, args)$data)
  }
  lrn2 = makePreprocWrapper(lrn, train = trainfun, predict = predictfun,
    par.set = par.set, par.vals = par.vals)
  lrn2$id = stri_replace(lrn2$id, replacement = ".multiRes", regex = "\\.preproc$")
  lrn2 = addClasses(lrn2, "MultiResFeaturesWrapper")
  return(lrn2)
}
