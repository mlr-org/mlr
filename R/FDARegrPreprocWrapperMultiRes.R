#' @title MultiRes preprocess wrapper
#'
#' @description Extracts multiResolution features and combine that with a learner 
#'
#' @export
makeFDARegrPreprocWrapperMultiRes = function(learner, ...) {
  lrn = checkLearner(learner)
  trainfun = function(data, target, args) {
    extractFDAMultiResFeatures = function(data, curve.lens, res.level = 3L, shift = 0.5) 
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
  lrn2 = makePreprocWrapper(lrn, train = trainfun, predict = predictfun, par.vals = list(...))
  lrn2$id = stri_replace(lrn2$id, replacement = ".multiRes", regex = "\\.preproc$")
  lrn2 = addClasses(lrn2, "MultiResFeaturesWrapper")
  return(lrn2)
}
