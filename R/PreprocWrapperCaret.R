#all purpose caret::preProcess Wrapper
makePreprocWrapperCaret = function (learner, par.set = NULL, ...) {
  
  trainfun = function(data, target, args) {
    requirePackages("caret", "train PreprocWrapperCaret")
    cns = colnames(data)
    work.cols = setdiff(cns, target) 
    x = data[, work.cols, drop = FALSE]
    mod = do.call(caret::preProcess, c(list(x = x), args))
    x = cbind.data.frame(predict(mod, data[, work.cols, drop = FALSE]), data[, setdiff(cns, work.cols), drop = FALSE])
    # print(head(x)); stop(123)
    list(data = x, control = mod)
  }
  
  predictfun = function(data, target, args, control) {
    requirePackages("caret", "predict PreprocWrapperCaret")
    data.frame(predict(control, data))
  }
  
  if (is.null(par.set)) {
    par.set = makeParamSet()
  }
    
  args = list(...)
  
  makePreprocWrapper(learner, trainfun, predictfun, par.set, par.vals = args)
}