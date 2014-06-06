#' @title Create model multiplexer for model selection to tune over multiple possible models.
#'
#' @description
#' Combines multiple base learners by dispatching
#' on the hyperparameter \dQuote{selected.learner} to a specific model class.
#' This allows to tune not only the model class (SVM, random forest, etc) but also
#' their hyperparameters in one go. Combine this with \code{\link{tuneParams}} and
#' \code{\link{makeTuneControlIrace}} for a very powerful approach.
#'
#' The parameter set is the union of all (unique) base learners.
#' In order to avoid name clashes all parameter names are prefixed
#' with the base learner id, i.e. \dQuote{[task.type].[learner.id].[parameter.name]}.
#'
#' @param base.learners [\code{list} of \code{\link{Learner}}]\cr
#'  List of Learners with unique IDs.
#' @param id [\code{character(1)}]\cr
#'  Identifier for constructed ModelMultiplexer.
#'  Default is \dQuote{ModelMultiplexer}.
#' @return [\code{ModelMultiplexer}]. A \code{\link{Learner}} specialized as \code{ModelMultiplexer}.
#' @aliases ModelMultiplexer
#' @export
#' bls = list( 
#'   makeLearner("classif.ksvm"),
#'   makeLearner("classif.randomForest")
#' )  
#' lrn = makeModelMultiplexer(bls)
#' rdesc = makeResampleDesc("CV", iters = 2L)
#' ps = makeParamSet(
#'   makeDiscreteParam("selected.learner", values = extractSubList(bls, "id")),
#'   makeNumerciParam("classif.ksvm.sigma", lower=-10, upper=10, requires = quote(selected.learner == "classif.ksvm")),
#'   makeIntegerParam("classif.randomForest.ntrees", lower=1, upper=500, requires = quote(selected.learner == "classif.randomForsest"))
#' )
#' ctrl = makeTuneControlIrace(maxit = 10)
#' res = tuneParams(lrn, iris.task, rdesc, par.set = ps, control = ctrl)
#' print(res)
#' print(head(as.data.frame(res$opt.path)))
makeModelMultiplexer = function(base.learners, id = "ModelMultiplexer") {
  checkArg(id, "character", len = 1L, na.ok = FALSE)
  checkArg(base.learners, "list", min.len = 1L)
  checkListElementClass(base.learners, "Learner")
  ids = unique(extractSubList(base.learners, "id"))
  if (length(ids) != length(base.learners))
    stop("Base learners must all have unique ids!")
  type = unique(extractSubList(base.learners, "type"))
  if (length(type) > 1L)
    stopf("Base learners must all be of same type, but have: %s", collapse(type))

  # construct combined param set
  par.set = makeParamSet(makeDiscreteLearnerParam("selected.learner", values=ids))
  for (i in seq_along(base.learners)) {
    ps = base.learners[[i]]$par.set
    pids = sprintf("%s.%s", ids[i], names(ps$pars))
    for (j in seq_along(ps$pars))
      ps$pars[[j]]$id = pids[[j]]
    names(ps$pars) = pids
    par.set = c(par.set, ps)
  }

  lrn = makeS3Obj(c("ModelMultiplexer", "Learner"),
    id = id,
    type = type,
    package = unique(extractSubList(base.learners, "package")),
    par.set = par.set,
    par.vals = list(selected.learner = ids[1L]),
    properties = Reduce(intersect, extractSubList(base.learners, "properties")),
    predict.type = "response"
  )

  lrn$base.learners = setNames(base.learners, ids)
  lrn
}

#' @export
trainLearner.ModelMultiplexer = function(.learner, .task, .subset, .weights, selected.learner, ...) {
  bl = .learner$base.learners[[selected.learner]]
  args = list(...)
  names(args) = substring(names(args), nchar(bl$id) + 2L)
  do.call(trainLearner, c(list(bl, .task, .subset, .weights), args))
}

#' @export
predictLearner.ModelMultiplexer = function(.learner, .model, .newdata, ...) {
  sl = .learner$par.vals$selected.learner
  bl = .learner$base.learners[[sl]]
  predictLearner(bl, .model, .newdata)
}
