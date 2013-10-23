#' Wrapper class for learners to handle multi-class problems. 
#' 
#' @exportClass MulticlassWrapper
#' @title Wrapper class for learners to handle multi-class problems.
setClass(
        "MulticlassWrapper",   
        contains = c("BaseWrapper")
)

#' Fuses a base learner with a multi-class method. Creates a learner object, which can be
#' used like any other learner object. This way learners which can only handle binary classification 
#' will be able to handle multi-class problems too.
#'
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param mcw.method [character(1) | function] \cr
#'   "onevsone" or "onevsrest". Default is "onevsrest".
#'   You can also pass a function, with signature \code{function(task)} and which
#'   returns a ECOC codematrix with entries +1,-1,0. 
#'   Columns define new binary problems, rows correspond to classes (rows must be named). 
#'   0 means class is not included in binary problem.   
#' @return \code{\linkS4class{Learner}}.
#' 
#' @title Fuse learner with multiclass method.
#' @export

makeMulticlassWrapper = function(learner, mcw.method="onevsrest") {
  if (is.character(learner))
    learner = makeLearner(learner)
  ps = makeParamSet(
    makeDiscreteLearnerParam(id="mcw.method", values=c("onevsone", "onevsrest"), default="onevsrest"),
    makeFunctionLearnerParam(id="mcw.custom")
  )
  w = new("MulticlassWrapper", learner=learner, par.set=ps)
  w@properties["multiclass"] = TRUE
  w@properties["prob"] = FALSE
  if (is.function(mcw.method)) {
    if (any(names(formals(mcw.method)) != c("task")))
      stop("Arguments in multiclass codematrix function have to be: task")   
    setHyperPars(w, mcw.custom=mcw.method)
  } else {
    setHyperPars(w, mcw.method=mcw.method)
  }
}


#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="MulticlassWrapper", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    pvs = .learner@par.vals
    
    .task = subsetData(.task, .subset)
    tn = .task@desc@target
    levs = .task@desc@class.levels
    d = getData(.task)
    y = getTargets(.task)
        
    if (is.null(pvs$mcw.custom)) { 
      meth = switch(pvs$mcw.method,
        onevsrest = cm.onevsrest,
        onevsone = cm.onevsone
      )
    } else{
      meth= pvs$mcw.custom
    }
    # build codematrix
    cm = meth(.task)
    if (!setequal(rownames(cm), levs))
      stop("Rownames of codematrix must be class levels!")
    if (!all(cm == 1 | cm == -1 | cm == 0))
      stop("Codematrix must only contain: -1,0,+1!")
    x = multi.to.binary(y, cm)
    
    # now fit models
    k = length(x$row.inds) 
    models = list()
    for (i in 1:k) {
      data2 = d[x$row.inds[[i]], ]
      data2[, tn] = x$targets[[i]] 
      ct = mlr:::changeData(.task, data2)
      models[[i]] = train(.learner@learner, ct)
    }
    # store cm as last el.
    models[[i+1]] = cm 
    return(models)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "MulticlassWrapper", 
    .model = "WrappedModel", 
    .newdata = "data.frame"
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    models = .model@learner.model
    cm = models[[length(models)]]
    k = length(models)-1
    p = matrix(0, nrow(.newdata), ncol=k)
    # we use hamming decoding here
    for (i in 1:k) {
      m = models[[i]]
      p[,i] = as.integer(as.character(predict(m, newdata=.newdata, ...)@df$response))
    }
    rns = rownames(cm)
    y = apply(p, 1, function(v) {
        # todo: break ties
        #j = which.min(apply(cm, 1, function(z) sum(abs(z - v))))
        d <- apply(cm, 1, function(z) sum(abs(z - v)))
        j <- which(d == min(d))
        j <- sample(rep(j,2), size = 1)
        rns[j]
      })
    as.factor(y)
  }
)   




# Function for Multi to Binary Problem Conversion
multi.to.binary = function(target, codematrix){
    
    if (any(is.na(codematrix)) ) {
        stop("Code matrix contains missing values!")
    }
    levs <- levels(target)
    no.class <- length(levs)
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
    n = length(task@desc@class.levels)
    cm = matrix(-1, n, n)
    diag(cm) = 1
    rownames(cm) = task@desc@class.levels
    return(cm)
} 

cm.onevsone = function(task) {
    n = length(task@desc@class.levels)
    cm = matrix(0, n, choose(n, 2))
    combs = combn(n, 2)
    for (i in 1:ncol(combs)) {
        j = combs[,i]
        cm[j, i] = c(1, -1) 
    }
    rownames(cm) = task@desc@class.levels
    return(cm)
} 

