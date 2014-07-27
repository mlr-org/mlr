#' @title Fuse learner with multiclass method.
#' 
#' @description
#' Fuses a base learner with a multi-class method. Creates a learner object, which can be used like any other learner object. This way learners which can only handle binary classification will be able to handle multi-class problems too.
#'
#' @template arg_learner 
#' @param mcw.method [character(1) | function] \cr
#'   "onevsone" or "onevsrest". Default is "onevsrest".
#'   You can also pass a function, with signature \code{function(task)} and which
#'   returns a ECOC codematrix with entries +1,-1,0. 
#'   Columns define new binary problems, rows correspond to classes (rows must be named). 
#'   0 means class is not included in binary problem.   
#' @template ret_learner
#' 
#' @export
makeMulticlassWrapper = function(learner, mcw.method="onevsrest") {
  learner = checkLearner(learner)
  ps = makeParamSet(
    makeDiscreteLearnerParam(id="mcw.method", values=c("onevsone", "onevsrest"), default="onevsrest"),
    makeFunctionLearnerParam(id="mcw.custom")
  )
  if (is.function(mcw.method)) {
    if (any(names(formals(mcw.method)) != c("task")))
      stop("Arguments in multiclass codematrix function have to be: task")   
    pv = list(mcw.custom=mcw.method)
  } else {
    pv = list(mcw.method=mcw.method)
  }
  
  id = paste(learner$id, "multiclass", sep = ".")
  
  x = makeBaseWrapper(id = id, next.learner = learner, package = learner$package, par.set = ps, par.vals = pv, cl = "MulticlassWrapper")
  addProperties(x, props = "multiclass")
  removeProperties(x, props = "prob")
  return(x)
}

#' @export
trainLearner.MulticlassWrapper = function(.learner, .task, .subset, .weights = NULL, mcw.method, mcw.custom, ...) {
  .task = subsetTask(.task, .subset)
  tn = .task$task.desc$target
  d = getTaskData(.task)
  y = getTaskTargets(.task)
  cm = buildCMatrix(mcw.custom = mcw.custom, mcw.method = mcw.method, .task = .task)
  x = multi.to.binary(y, cm)
  # now fit models
  k = length(x$row.inds) 
  models = lapply(seq_len(k), function(i) {
    data2 = d[x$row.inds[[i]], ]
    data2[, tn] = x$targets[[i]] 
    ct = changeData(.task, data2)
    train(.learner$next.learner, ct)
  })
  makeChainModel(next.model = list(models = models, cm = cm), cl = "MulticlassModel")
}


#' @export 
predictLearner.MulticlassModel = function(.learner, .model, .newdata, ...) {
  models = .model$learner.model$next.model$models
  cm = .model$learner.model$next.model$cm
  # we use hamming decoding here
  p = sapply(models, function(m) {
    nd = .newdata[, m$features, drop = FALSE]
    as.integer(predict(m, newdata = nd, ...)$data$response)
  })
  rns = rownames(cm)
  y = apply(p, 1, function(v) {
    # todo: break ties
    #j = which.min(apply(cm, 1, function(z) sum(abs(z - v))))
    d = apply(cm, 1, function(z) sum(abs(z - v)))
    j = which(d == min(d))
    j = sample(rep(j,2), size = 1)
    rns[j]
  })
  as.factor(y)
}

#' @export
makeWrappedModel.MulticlassWrapper = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod()
  addClasses(x, "MulticlassModel")
}


buildCMatrix = function (mcw.custom, mcw.method, .task) {
  # build codematrix
  if (isSet(mcw.custom)) {
    meth = mcw.custom
  } else {
    meth = switch(mcw.method,
                  onevsrest = cm.onevsrest,
                  onevsone = cm.onevsone)
  }
  levs = getTaskFactorLevels(.task)[[1]]
  cm = meth(.task)
  if (!setequal(rownames(cm), levs))
    stop("Rownames of codematrix must be class levels!")
  if (!all(cm == 1 | cm == -1 | cm == 0))
    stop("Codematrix must only contain: -1,0,+1!")
  cm
}

# Function for Multi to Binary Problem Conversion
multi.to.binary = function(target, codematrix){
  
  if (any(is.na(codematrix)) ) {
    stop("Code matrix contains missing values!")
  }
  levs = levels(target)
  no.class = length(levs)
  rns = rownames(codematrix)
  if (is.null(rns) || !setequal(rns, levs)) {
    stop("Rownames of code matrix have to be the class levels!")
  }
  
  binary.targets = as.data.frame(codematrix[target,])
  row.inds = lapply(binary.targets, function(v) which(v != 0))
  names(row.inds) = NULL
  targets = Map(function(y, i) factor(y[i]),
                binary.targets, row.inds)
  
  return(list(row.inds=row.inds, targets=targets))
}

cm.onevsrest = function(task) {
  n = length(task$task.desc$class.levels)
  cm = matrix(-1, n, n)
  diag(cm) = 1
  rownames(cm) = task$task.desc$class.levels
  return(cm)
} 

cm.onevsone = function(task) {
  n = length(task$task.desc$class.levels)
  cm = matrix(0, n, choose(n, 2))
  combs = combn(n, 2)
  for (i in 1:ncol(combs)) {
    j = combs[,i]
    cm[j, i] = c(1, -1) 
  }
  rownames(cm) = task$task.desc$class.levels
  return(cm)
} 

