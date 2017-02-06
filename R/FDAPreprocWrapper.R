#' @title Functional data preproc wrapper
#'
#' @description Extracts  functional data features like wavelets, fourier, etc.
#'   and creates a learner structure for applicable learners of type \code{classif}.
#'
#' @export
makeFDAPreprocWrapper = function(learner) {
  lrn = checkLearner(learner)

  trainfun = function(data, target, args) {
    taskTs = makeFDAClassifTask(data = data, target = target, positive = args$positive)
    taskFa = convertFDATaskToNormalTask(task = taskTs)
    A = getTaskData(taskFa)
    control = list()
    list(data = A, control = control) # Preprocessing train must return a list with elements data[data.frame] and control[list]!
  }

  predictfun = function(data, target, args, control) {
    return(trainfun(data, target, args)$data)
    #y = intersect(target, colnames(data))
    #data = do.call(control$fun, c(list(curves = data), args))
    #return(data)
  }

  require('stringi')
  lrn2 = makePreprocWrapper(lrn, train = trainfun, predict = predictfun, par.vals = list(positive = "1"))
  lrn2$id = stri_replace(lrn2$id, replacement = ".fourier", regex = "\\.preproc$")
  lrn2 = addClasses(lrn2, "FourierFeaturesWrapper")
  return(lrn2)
}
