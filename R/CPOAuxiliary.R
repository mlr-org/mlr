#' @title CPO: Composable Preprocessing Operators
#'
#' @description
#' FIXME to come
#'
#' @family CPO
#' @name CPO
NULL



### Generics

#' @title CPO Composition / Attachment operator
#'
#' @description
#' This operator \dQuote{pipes} data from the source into the target object.
#'
#' If both objects are a \link{CPO} object, they will be composed. A new object,
#' representing the operation of performing both object's operations in succession,
#' will be created, which can be handled like a new \link{CPO} object.
#'
#' If the left object is a \code{data.frame} or a \code{link{Task}}, the
#' transformation operation will be applied to this data, and the same resulting
#' data will be returned.
#'
#' If the right object is a \code{\link{Learner}}, the CPO will be attached to
#' this learner. The same operation will be performed during the \dQuote{train} and
#' \dQuote{predict} phase; the behaviour during the predict phase may furthermore
#' be depend on the training data.
#'
#' Note that you can not link a \code{data.frame} or \code{\link{Task}} directly
#' to a \code{\link{Learner}}, since this operation is not algebraically associative
#' with the composition of CPOs. Use \code{\link{train}} for this.
#'
#' @param cpo1 [\code{data.frame} | \code{\link{Task}} | \code{\link{CPO}}]\cr
#'   The source object.
#' @param cpo2 [\code{\link{CPO}} | \code{\link{Learner}}]\cr
#'   The target object.
#'
#' @family CPO
#' @examples
#' # PCA-rotate pid.task
#' rotated.pid.task = pid.task %>>% cpoPca()
#'
#' # Centering / Scaling *after* PCA
#' neoPCA = cpoPca(center = FALSE, scale = FALSE, id = "pca") %>>% cpoScale()
#'
#' # Attach the above to learner
#' pcaLogreg = neoPCA %>>% makeLearner("classif.logreg")
#'
#' @export
`%>>%` = function(cpo1, cpo2) {
  UseMethod("%>>%")
}

#' @export
`%>>%.default` = function(cpo1, cpo2) {
  stopf("%%>>%% not defined for objects of class c(%s)", paste0('"', class(cpo1), '"', collapse = ", "))
}

#' @export
`%>>%.data.frame` = function(cpo1, cpo2) {
  name = deparse(substitute(cpo1), 20)[1]
  task = makeClusterTask(name, cpo1)
  retrafo(task) = retrafo(cpo1)
  resulttask = task %>>% cpo2
  result = getTaskData(resulttask)
  retrafo(result) = retrafo(resulttask)
  result
}

#' @export
`%>>%.Task` = function(cpo1, cpo2) {
  if ("Learner" %in% class(cpo2)) {
    stopf("%s\n%s\n%s\n%s",
      "Cannot pipe data into learner!",
      "If you called 'data %>>% preproc %>>% learner', you probably meant",
      "train(preproc %>>% learner, data). Note that this is different from",
      "'train(learner, data %>>% preproc), which is usually not what you want.")
  } else if ("CPO" %in% class(cpo2)) {
    applyCPO(cpo2, cpo1)
  } else if ("CPORetrafo" %in% class(cpo2)) {
    predict(cpo2, cpo1)
  } else if ("CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
  } else {
    stopf("Cannot compose data with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}


#' @export
`%>>%.CPOConstructor` = function(cpo1, cpo2) {
  stop("Cannot compose CPO Constructors.")
}

#' @export
`%>>%.CPO` = function(cpo1, cpo2) {
  if ("CPO" %in% class(cpo2)) {
    # compose two CPOs
    composeCPO(cpo1, cpo2)
  } else if ("Learner" %in% class(cpo2)) {
    # wrap around learner
    attachCPO(cpo1, cpo2)
  } else if ("CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Constructors.\nDid you forget to construct the CPO?")
  } else {
    stopf("Cannot compose CPO with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

#' @title CPO Composition
#'
#' @description
#' The arguments will be composed. A new object,
#' representing the operation of performing both object's operations in succession,
#' will be created, which can be handled like a new \link{CPO} object.
#'
#' See the preferred \code{\link{\%>>\%}} for more info.
#'
#' @param cpo1 [\code{\link{CPO}}]\cr
#'   The operation to perform first.
#' @param cpo2 [\code{\link{CPO}}]\cr
#'   The operation to perform second.
#'
#' @export
composeCPO = function(cpo1, cpo2) {
  UseMethod("composeCPO")
}

#' @title CPO Attachment
#'
#' @description
#' The second argument is a \code{\link{Learner}} and the CPO will be attached to
#' this learner. The same operation will be performed during the \dQuote{train} and
#' \dQuote{predict} phase; the behaviour during the predict phase may furthermore
#' be depend on the training data.
#'
#' See the preferred \code{\link{\%>>\%}} for more info.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO object
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#'
#' @family CPO
#'
#' @export
attachCPO = function(cpo, learner) {
  UseMethod("attachCPO")
}

#' @title CPO Apply
#'
#' @description
#' The given transformation will be applied to the data in the given \code{link{Task}}.
#'
#' See the preferred \code{\link{\%>>\%}} for more info.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO representing the operation to perform.
#' @param task [\code{\link{Task}}]\cr
#'   The task to operate on.
#'
#' @family CPO
#' @export
applyCPO = function(cpo, task) {
  UseMethod("applyCPO")
}

#' @title Get the CPO object's Name
#'
#' @description
#' Return the name given at creation as \dQuote{.cpo.name} to the
#' CPO creator. If the CPO object has an ID, it will be appended.
#'
#' @param cpo [\code{\link{CPO}}]\cr
#'   The CPO object.
#'
#' @family CPO
#' @export
getCPOName = function(cpo) {
  UseMethod("getCPOName")
}

#' @title Get the Retransformation function from a resulting object
#'
#' @description
#' When applying a CPO to a \code{data.frame} or \code{\link{Task}},
#' the data is not only changed, additionally a retransformation
#' function is created that can be applied to other data of the same
#' kind.
#'
#' For example, when performing PCA on training data, the rotation
#' matrix is saved and can be used on new (prediction) data.
#'
#' \dQuote{retrafo} retrieves a function that can be applied to new
#' data sets and \code{Task}s.
#'
#' When chaining \code{\link{\%>>\%}} on a data object, the retrafo
#' associated with the result is also chained automatically. Beware,
#' however, that this just accesses the retrafu function with
#' \code{retrafo} internally. Therefore, if you plan to do apply
#' multiple transformations with certain operations in between,
#' make sure to reset the retrafo function by setting it to \code{NULL}.
#' See examples.
#'
#' @param data [\code{data.frame} | \code{\link{Task}} | \code{\link{WrappedModel}}]\cr
#'   The result of a \code{\link{\%>>\%}} chain applied to a data set.
#' @param default.to.identity [\code{logical}]\cr
#'   Whether to return the identity function when no trafo was found.
#'   Default is \code{FALSE}.
#'
#' @return [\code{function}]. The retransformation function that can be
#' applied to new data.
#'
#' @examples
#'
#' traindat = subsetTask(pid.task, 1:400)
#' preddat = subsetTask(pid.task, 401:768)
#'
#' trained = traindat %>>% cpoPca()
#' reFun = retrafo(trained)
#' predicted = reFun(preddat)
#'
#' # chaining works
#' trained = traindat %>>% cpoPca(FALSE, FALSE) %>>% cpoScale()
#' reFun = retrafo(trained)
#' predicted = reFun(preddat)
#'
#' # reset the retrafo when doing other steps!
#' \dontrun{
#' trained.tmp = traindat %>>% cpoPca(FALSE, FALSE)
#' reFun1 = retrafo(trained.tmp)
#'
#' imp = impute(trained.tmp)
#' trained.tmp = imp$task  # nonsensical example
#' retrafo(trained.tmp) = NULL  # NECESSARY HERE
#'
#' trained = trained.tmp %>>% cpoScale()
#'
#' reFun2 = retrafo(trained)
#' predicted = reFun2(getTaskData(reimpute(
#'   reFun1(preddat), imp$desc), target.extra = TRUE)$data)
#'
#'
#' }
#' @family CPO
#' @export
retrafo = function(data, default.to.identity = FALSE) {
  UseMethod("retrafo")
}

#' @export
retrafo.WrappedModel = function(data, default.to.identity = FALSE) {
  warning("retrafo of a model not available if the outermost wrapper was not a CPO.")
  if (default.to.identity) {
    identity
  } else {
    NULL
  }
}


# default.to.identity is ignored, since a CPOModel always has a retrafo
#' @export
retrafo.CPOModel = function(data, default.to.identity = FALSE) {
  # go through the chained model and see if there are wrapped models that
  # are not %>>%-chained (since the user probably wants to be warned about
  # that.
  recurseRetrafo = function(model, prevfun) {
    resfun = singleModelRetrafo(model, prevfun)
    next.model = model$learner.model$next.model
    if ("BaseWrapperModel" %in% class(next.model)) {
      if ("CPOModel" %in% class(next.model)) {
        return(recurseRetrafo(next.model, resfun))
      }
      do.message = FALSE
      while (!is.null(next.model)) {
        if (!is.list(next.model$learner.model)) {
          break
        }
        next.model = next.model$learner.model$next.model
        if ("CPOModel" %in% class(next.model)) {
          warningf("The model apparently has some CPOs wrapped by other wrappers\n%s\n%s",
            "The resulting retrafo will only cover the operations up to",
            "the first non-CPO wrapper!")
          do.message = FALSE
          break
        }
        if (!"BaseWrapperModel" %in% class(next.model)) {
          do.message = TRUE
        }
      }
      if (do.message) {
        message("The model has some wrappers besides CPOs, which will not be part of the retrafo.")
      }
    }
    resfun
  }
  recurseRetrafo(data, NULL)
}

singleModelRetrafo = function(model, prevfun) {
  UseMethod("singleModelRetrafo")
}

#' @export
retrafo.default = function(data, default.to.identity = FALSE) {
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("data is not a Task or data.frame.\n%s\n%s",
      "are you sure you are applying it to the result",
      "of a %>>% transformation?")
  }
  res = attr(data, "retrafo")
  if (default.to.identity && is.null(res)) {
    identity
  } else {
    res
  }
}



#' @title set an object's retransformation
#'
#' @description
#' Set an object's retransformation function, as described
#' in \code{\link{retrafo}}. Set to \code{NULL} to delete.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The task of which to set the retrafo.
#' @param value [\code{function} | NULL]\cr
#'   The retrafo function to set. This must either be a
#'   function accepting a \code{data.frame} and returning
#'   an object of the same kind, or NULL.
#'   In most cases, you should use this only within
#'   \code{CPOFunctionalConstructor} functions OR to
#'   reset an object's retrafo to NULL.
#'
#' @family CPO
#' @export
`retrafo<-` = function(data, value) {
  UseMethod("retrafo<-")
}


#' @export
`retrafo<-.WrappedModel` = function(data, value) {
  stop("Cannot change retrafo of a model!")
}


#' @export
`retrafo<-.default` = function(data, value) {
  if (!is.null(value)) {
    assertClass(value, "CPORetrafo")
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")

  }
  attr(data, "retrafo") = value

  data
}

#' @title Get the internal state of a Retrafo object
#'
#' @description
#' A retrafo function always has access to some kind of state
#' that represents information gotten from the training data,
#' as well as the parameters it was called with.
#'
#' The structure of the internal state depends on the CPO backend
#' used. For Functional CPO, the state is the environment of the
#' retrafo function, turned into a list. For Objectbased CPO,
#' the state is a list containing the parameters, as well as the
#' control object generated by the trafo function.
#'
#' The object can be slightly modified and used to create a new
#' CPO retrafo object using \code{\link{makeRetrafoFromState}}.
#'
#' @param retrafo.object [\code{CPORetrafo}]\cr
#'   The object to get the state of.
#'
#' @return a list.
#' @family CPO
#' @export
getRetrafoState = function(retrafo.object) {
  UseMethod("getRetrafoState")
}

#' @title Set the internal state of a Retrafo object
#'
#' @description
#' This creates a new \code{Retrafo} object which will
#' behave according to \dQuote{state}.
#'
#' @param constructor
#'   A cpo constructor
#' @param state
#'   A state gotten from another CPO retrafo object using
#'   \code{\link{getRetrafoState}}
#' @return a \code{CPORetrafo}.
#' @family CPO
#' @export
makeRetrafoFromState = function(constructor, state) {
  UseMethod("makeRetrafoFromState")
}

setCPOId = function(cpo, id) {
  UseMethod("setCPOId")
}

setCPOId.default = function(cpo, id) {
  stop("setCPOId for object not defined.")
}

#' @title Turn a list of preprocessing operators into a single chained one
#'
#' @description
#' Chain a list of preprocessing operators, or retrafo objects, turning \code{list(a, b, c)} into
#' \code{a \%>>\% b \%>>\% c}. This is the inverse operation of \code{as.list},
#' applied on a \code{CPO} chain.
#'
#' @param pplist [\code{list} of \code{CPO} | \code{list} of \code{CPORetrafo}]\cr
#'   A list of \code{CPO} or \code{CPORetrafo} objects.
#'
#' @family CPO
#' @export
chainCPO = function(pplist) {
  assert(checkList(pplist, types = "CPO", min.len = 1),
    checkList(pplist, types = "CPORetrafo", min.len = 1))
  Reduce(`%>>%`, pplist)
}

#' @export
setHyperPars2.CPORetrafo = function(learner, par.vals = list()) {
  stopf("Cannot change parameter values of retrafo object\n%s\n%s\n",
    "To create a retrafo with a specific state use makeRetrafoFromState.",
    "Get the state of an existing retrafo using getRetrafoState.")
}

### Printing

#' @export
print.CPOConstructor = function(x, ...) {
  args = formals(x)
  argvals = sapply(args, function(y) if (identical(y, substitute())) "" else paste(" =", deparseJoin(y, "\n")))
  argstring = paste(names(args), argvals, collapse = ", ", sep = "")
  catf("<<CPO %s(%s)>>", environment(x)$cpo.name, argstring)
}


#' @export
print.CPO = function(x, ...) {
  pv = getHyperPars(x)
  argstring = paste(names(pv), sapply(pv, deparseJoin, sep = "\n"), sep = " = ", collapse = ", ")
  template = ifelse("CPOPrimitive" %in% class(x), "%s(%s)", "(%s)(%s)")
  catf(template, getCPOName(x), argstring)
}

#' @export
print.DetailedCPO = function(x, ...) {
  NextMethod("print", x)
  cat("\n")
  print(getParamSet(x))
}

#' @export
summary.CPOObject = function(object, ...) {
  if (!"DetailedCPO" %in% object) {
    class(object) = c(head(class(object), -1), "DetailedCPO", "CPO")
  }
  object
}

#' @export
print.CPORetrafo = function(x, ...) {
  first = TRUE
  for (primitive in as.list(x)) {
    if (!first) {
      cat(" =>\n")
    }
    first = FALSE
    pv = getHyperPars(primitive)
    argstring = paste(names(pv), sapply(pv, deparseJoin, sep = "\n"), sep = " = ", collapse = ", ")
    catf("[RETRAFO %s(%s)]", getCPOName(primitive), argstring, newline = FALSE)
  }
  cat("\n")
}

### Auxiliaries

# deparseJoin: deparse, but work with longer than 500 char expressions, mostly.
# Note that this is a heuristic for user messages only, the result can not be
# parsed again!
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

getParamSetDefaults = function(ps) {
  lapply(ps$pars[vlapply(ps$pars, function(x) x$has.default)], function(x) x$default)
}

# check that ParamSets  ps1 and ps2 have distinct names; if not, give meaningful
# error message, referring to the objects by name1 and name2.
parameterClashAssert = function(obj1, obj2, name1, name2) {
  ps1 = getParamSet(obj1)
  ps2 = getParamSet(obj2)
  samenames = intersect(names(ps1$pars), names(ps2$pars))
  if (length(samenames)) {
    plur = length(samenames) > 1
    stopf("Parameter%s %s occur%s in both %s and %s\n%s", ifelse(plur, "s", ""),
      paste0('"', samenames, '"', collapse = ", "), ifelse(plur, "", "s"), name1, name2,
      "Use the id parameter when constructing, or setCPOId, to prevent name collisions.")
  }
}

noMissingAssert = function(paramlist) {
  lapply(names(paramlist), function(x) {
    if (identical(paramlist[[x]], substitute())) {
      stopf("Parameter %s missing, with no default.", x)
    }
  })
}

getLearnerName = function(learner) {
  coalesce(learner$name, learner$shortname, learner$id)
}


subsetParams = function(par.vals, par.set, name) {

  # these parameters are either present or have fulfilled requirements
  needed = names(Filter(function(x) {
    x$id %in% names(par.vals) ||
          is.null(x$requires) || isTRUE(try(eval(x$requires, envir = par.vals), silent = TRUE))
  }, par.set$pars))

  present = names(par.vals)

  missing.pars = setdiff(needed, present)
  if (length(missing.pars)) {
    plur = length(missing.pars) > 1
    stopf("Parameter%s %s of CPO %s %s missing\n%s", ifelse(plur, "s", ""),
      collapse(missing.pars, sep = ", "), name, ifelse(plur, "are", "is"),
      "Either give it during construction, or with setHyperPars.")
  }

  par.vals[needed]
}

checkParamsFeasible = function(par.set, par.vals) {
  # names(par.vals) must be a subset of names(par.set$pars)
  oobreaction = coalesce(getMlrOption("on.par.out.of.bounds"), TRUE)
  if (oobreaction != "quiet") {
    for (n in names(par.vals)) {
      if (!isFeasible(par.set$pars[[n]], par.vals[[n]])) {
        msg = sprintf("%s is not feasible for parameter '%s'!", convertToShortString(par.vals[[n]]), n)
        if (oobreaction == "stop") {
          stop(msg)
        } else {
          warning(msg)
        }
      }
    }
  }
}

renameNonfunctionNames = function(expr, translate) {
  startfrom = 1
  if (is.call(expr)) {
    if (!is.recursive(expr)) {
      return(expr)
    }
    startfrom = 2
    if (is.recursive(expr[[1]])) {
      startfrom = 1
    } else if (length(expr) == 1) {
      return(expr)
    }
  }
  if (is.recursive(expr)) {
    for (idx in seq(startfrom, length(expr))) {
      expr[[idx]] = renameNonfunctionNames(expr[[idx]], translate)
    }
  } else if (is.symbol(expr) && as.character(expr) %in% names(translate)) {
    expr = as.symbol(translate[[as.character(expr)]])
  }
  return(expr)
}


makeFunction = function(expr, required.arglist, env = parent.frame()) {
  if (is.recursive(expr) && identical(expr[[1]], quote(`{`))) {
    # we have a headless list of expressions
    # so we build our own function
    args = as.pairlist(required.arglist)
    newfun = eval(call("function", args, expr), envir = env)
  } else {
    newfun = eval(expr, envir = env)
    assertFunction(newfun)
    if (!"..." %in% names(formals(newfun))) {
      # with a vararg function we tentatively trust the function
      # handles its arguments

      assertFunction(newfun, args = names(required.arglist))
    }
    for (arg in names(formals(newfun))) {
      if (arg == "...") {
        # can't say anything about ellipses
        next
      }
      if (identical(formals(newfun)[[arg]], substitute())) {
        # the argument has no default, so we only care that this it will always be given
        if (!arg %in% names(required.arglist)) {
          stopf("Bad argument %s.\nNeed a function with arguments %s.", arg, collapse(names(required.arglist)), sep = ", ")
        }
        next
      }
      if (!arg %in% names(required.arglist)) {
        # the argument is not required, but it has a default; that is fine.
        next
      }
      if (!identical(formals(newfun)[[arg]], required.arglist[[arg]])) {
        if (identical(formals(newfun)[[arg]], substitute())) {
          funargstr = "no default"
          funarg = "no default"
        } else {
          funarg = eval(formals(newfun)[[arg]], envir = env)
          funargstr = sprintf("default %s", collapse(funarg))
        }
        if (identical(required.arglist[[arg]], substitute())) {
          reqarg = "no default"
        } else {
          reqarg = required.arglist[[arg]]
        }
        # there is a chance that they evaluate to the same value, so we check again
        if (!identical(funarg, reqarg) && (!length(funarg) == 1 || !length(reqarg) == 1 || !is.na(funarg) || !is.na(reqarg))) {
          stopf("Given function parameter %s has %s, but required default is %s.",
                arg, funargstr, collapse(reqarg))
        }
      }
    }
  }
  newfun
}

# capture the environment of the call to 'fun'
captureEnvWrapper = function(fun) {
  envcapture = quote({ assign(".ENV", environment(), envir = environment(sys.function())) ; 0 })
  envcapture[[3]] = body(fun)
  body(fun) = envcapture
  environment(fun) = new.env(parent = environment(fun))
  fun
}



#' @export
removeHyperPars.CPOLearner = function(learner, ids) {
  i = intersect(names(learner$par.vals), ids)
  if (length(i) > 0) {
    stopf("CPO Parameters (%s) can not be removed", collapse(i, sep = ", "))
  }
  learner$next.learner = removeHyperPars(learner$next.learner, ids)
  learner
}

#' @export
as.list.CPOPrimitive = function(x, ...) {
  assert(length(list(...)) == 0)
  list(x)
}


#' @export
getRetrafoState.CPORetrafo = function(retrafo.object) {
  stop("Cannot get state of compound retrafo. Use as.list to get individual elements")
}

#' @export
getParamSet.CPORetrafo = function(x) {
  stop("Cannot get param set of compound retrafo. Use as.list to get individual elements")
}


#' @export
getHyperPars.CPORetrafo = function(learner, for.fun = c("train", "predict", "both")) {
  stop("Cannot get parameters of compound retrafo. Use as.list to get individual elements")
}

#' @export
getCPOName.CPORetrafo = function(cpo) {
  paste(sapply(as.list(cpo), getCPOName), collapse = " => ")
}

# TO-DO:
#- remove 'data' from function environment
#- properties
#- taskstyle: whether to get the task, data as whole df, data sep from target, data sep into types
#- task disassemble / assemble methods
#- column names
#- core rewrite
#- attach retrafo to model
#- retrafo: need to remove target
# --> nothing happens for empty features
# -->
#- apply retrafo to prediction
# --> have parameter 'targetbound' --> must not change data, but may change target, will have empty retrafo but meaningful predictTrafo
#- remove retrafo from learner
#- functional retrafo: check for data reference and warn

splitColsByType = function(which = c("numeric", "factor", "ordered", "other"), data, types) {
  if (missing(types)) {
    types = vcapply(data, function(x) class(x)[1])
  }
  # types: may be a character of type names, then data can be something besides a data.frame, like just a vector of names or indices
  match.arg(which, several.ok = TRUE)
  factorsubset = c("factor", if (!"ordered" %in% which) "ordered")

  sapply(which, function(x) {
    subset = if (x == "other") {
               !types %in% c("integer", "numeric", "factor", "ordered")
             } else {
               types %in% switch(x,
                 numeric = c("integer", "numeric"),
                 factor = factorsubset,
                 ordered = "ordered")
             }
    data[subset]
  }, simplify = FALSE, USE.NAMES = TRUE)
}

# this performs no checks. possibly need to check that properties are adhered to
# in retrafo, must also check if the format is the same as during training
# 'possibly' here means: if not attached to a learner
splittask = function(task, datasplit = c("target", "most", "all", "no", "task")) {
  datasplit = match.arg(datasplit)


  if (datasplit %in% c("most", "all")) {
    splt = getTaskData(task, target.extra = TRUE)

  }

  switch(datasplit,
    task = list(data = task, target = getTaskTargetNames(task)),
    no = list(data = getTaskData(task), getTaskTargetNames(task)),
    target = list(data = getTaskData(task, target.extra = TRUE)$data,
     target = getTaskData(task, features = character(0))),  # want the target to always be a data.frame
    most = list(data = splitColsByType(c("numeric", "factor", "other"), splt$data),
      target = splt$target),
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), splt$data),
      target = splt$target))
}

splitdf = function(df, datasplit = c("target", "most", "all", "no", "task")) {
  datasplit = match.arg(datasplit)
  switch(datasplit,
    task = list(data = makeClusterTask(data = df), target = character(0)),
    no = list(data = df, character(0)),
    target = list(data = df, target = df[, character(0), drop = FALSE]),
    most = list(data = splitColsByType(c("numeric", "factor", "other"), df),
      target = df[, character(0), drop = FALSE]),
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), df),
      target = df[, character(0), drop = FALSE]))
}

recombineLL = function(olddata, allnames, targetdata, name) {
# need 'allnames' to preserve ordering of target columns within whole DF
  tnames = names(targetdata)

  dfs = sapply(newdata, is.data.frame)
  if (any(!dfs)) {
    is.plur = sum(!dfs) > 1
    stopf("Return of %s element%s %s %s not a data.frame.", name, ifelse(is.plur, "s", ""),
      collapse(names(dfs)[!dfs], sep = ", "), ifelse(is.plur, "are", "is"))
  }

  # check no new names clash with other names
  # this kind of sucks when a CPO just happens to change the names to something thats already there
  # but we also don't want to surprise the user about us unilaterally changing names, so he needs to
  # take care of that.
  allnames = c(tnames, unlist(lapply(newdata, names)))
  if (any(duplicated(allnames))) {
    stopf("CPO %s gave bad result\nduplicate column names %s", name, collapse(unique(allnames[duplicated(allnames)], sep = ", ")))
  }

  types = vcapply(olddata, function(x) class(x)[1])

  splitnames = splitColsByType(names(newdata), names(olddata), types) # list(numeric = [colnames], factor = [colnames]...

  numrows = nrow(olddata)
  namesorder = allnames
  for (splittype in names(splitnames)) {
    if (numrows != nrow(newdata[[splittype]])) {
      stopf("Number of rows of %s data returned by %s did not match input\nCPO must not change row number.",
        splittype, name)
    }
    if (!identical(splitnames[[splittype]], names(newdata[[splittype]]))) {
        namesorder = setdiff(originalorder, c(splitnames[[splittype]], tnames))
        namesorder = c(namesorder, c(names(newdata[[splittype]]), tnames))
    }
  }

  newdata = cbind(do.call(cbind, unname(newdata)), targetdata)
  assertSetEqual(names(newdata), namesorder)
  newdata[namesorder]
}

# this checks that the result has the proper type, that target and type didn't change
# (if datasplit == "task"), and that the number of rows is the same.
# checking that the properties are adhered to must also happen (somewhere else)
# in retrafo functions, it must also be checked that the number of columns matches the one
# seen after trafo.
recombinetask = function(task, newdata, datasplit = c("target", "most", "all", "no", "task"), name) {
  datasplit = match.arg(datasplit)

  assertTargetsEqual = function(old.targets, new.targets) {
    assertSetEqual(names(old.targets), names(new.targets))
    for (n in names(old.targets)) {
      if (any(old.targets[[n]] != new.targets[[n]])) {
        stopf("CPO %s must not change target, but changed %s.", name, n)
      }
    }
  }


  if (datasplit %in% c("no", "task")) {
    if (datasplit == "no") {
      if (!is.data.frame(newdata)) {
        stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame.", name)
      }
      assertClass(newdata, "data.frame")
      newdata = changeData(task, newdata)
    }
    assertClass(newdata, "Task")
    #check type didn't change
    assert(getTaskType(task) == getTaskType(newdata))

    # check target didn't change
    assertTargetsEqual(getTaskData(task, features = character(0)),
      getTaskData(newdata, features = character(0)))

    # check most of task description didn't change
    allowed.td.changes = c("id", "n.feat", "has.missings")
    old.td = getTaskDesc(task)
    new.td = getTaskDesc(newdata)
    assertSetEqual(names(old.td), names(new.td))
    for (n in names(old.td)) {  # implicitly checks row number
      if (!n %in% allowed.td.changes && !identical(old.td[[n]], new.td[[n]])) {
        stopf("CPO %s changed task description item %s.", name, n)
      }
    }

    return(task)
  }
  if (datasplit == "target") {
    if (!is.data.frame(newdata)) {
      stopf("CPO %s gave bad result\nmust return a data.frame.", name)
    }
    if (nrow(newdata) != getTaskDescription(task)$size) {
      stopf("CPO %s must not change number of rows.", name)
    }
    newdata = cbind(newdata, getTaskData(task, features = character(0)))
    if (identical(names(newdata), getTaskFeatureNames(task))) {
      # names didn't change, so we preserve column order
      newdata = newdata[names(task$env$data)]
    }

  } else {
    # datasplit is 'most' or 'all'
    assertSetEqual(names(newdata), c("numeric", "factor", "other", if (datasplit == "all") "ordered"))

    target = getTaskData(task, features = character(0))

    newdata = recombineLL(getTaskData(task, target.extra = TRUE)$data,
      names(task$env$data), target, name)
  }
  changeData(task, newdata)
}

# this assumes we that if 'datasplit' was task, it was created from a target-less task.
recombinedf = function(df, newdata, datasplit = c("target", "most", "all", "no", "task"), name) {
  datasplit = match.arg(datasplit)
  assertSetEqual(names(newdata), c("numeric", "factor", "other", if (datasplit == "all") "ordered"))
  if (datasplit == "task") {
    assertClass(newdata, "Task")
    getTaskData(newdata)
  } else if (datasplit %in% c("most", "all")) {
    return(recombineLL(df, names(df), df[, character(0), drop = FALSE], name))
  } else {
    if (nrow(df) != nrow(newdata)) {
      stopf("CPO %s must not change number of rows.", name)
    }
    newdata
  }
}


# prepare some information about the data shape, so retrafo can check that
# it gets the kind of data it expects
# this needs to be checked both for input and for output
makeShapeInfo = function(data) {
  # expects a data.frame
  prep.info = list()
  prep.info$colnames = colnames(data)
  prep.info$coltypes = vcapply(data, function(x) class(x)[1])
  prep.info
}

# like makeShapeInfo, but additionally get the target names
makeInputShapeInfo = function(indata) {
  if ("Task" %in% class(indata)) {
    target = getTaskTargetNames(indata)
    indata = getTaskData(indata, target.extra = TRUE)$data
    ret = makeShapeInfo(indata)
    ret$target = target
  } else {
    ret = makeShapeInfo(indata)
  }
  ret
}

# called before recombinetask for better error messages later
# it is recommended to call this after recombinetask was called
# (but with the not yet recombined data)
# since that checks that outdata has the correct types.
makeOutputShapeInfo = function(outdata) {
  if (is.data.frame(outdata)) {
    makeShapeInfo(outdata)
  } else if ("Task" %in% class(outdata)) {
    makeShapeInfo(getTaskData(outdata))
  } else {
    # data is split by type, so we get the shape of each of the constituents
    lapply(outdata, makeShapeInfo)
  }
}

# give error when shape is different than dictated by shapeinfo.
assertShapeConform(df, shapeinfo, checkordered, name) {
  assertSubsetEqual(names(indata), shapeinfo$colnames)
  indata = indata[shapeinfo$colnames]

  if (checkordered) {
    typesmatch = list(
        c("integer", "numeric"),
        "factor", "ordered")
  } else {
    typesmatch = list(
        c("integer", "numeric"),
        c("factor", "ordered"))
  }

  newcoltypes = vcapply(indata, function(x) class(x)[1])

  for (t in typesmatch) {
    typemismatch = (newcoltypes %in% t) != (newcoltypes %in% shapeinfo$coltypes)
    if (any(typemismatch)) {
      stopf("Error in CPO %s: Types of column%s %s mismatch.", name,
        ifelse(sum(typemismatch) > 1, "s", ""), collapse(names(indata)[typemismatch], sep = ", "))
    }
  }
}

checkRetrafoInput = function(indata, checkordered, shapeinfo, name) {
  # check that input column names and general types match (num / fac, or num/fac/ordered if datasplit == "all"
  #
  # how does mlr predict handle this stuff? they just drop target columns by name

  if ("Task" %in% class(indata)) {
    if (!is.null(shapeinfo$target)) {
      assertSetEqual(getTaskTargetNames(indata), shapeinfo$target)
    }
    indata = getTaskData(indata)
  }
  if (!is.data.frame(indata)) {
    stopf("Data fed into CPO %s retrafo is not a data.frame.", name)
  }

  assertShapeConform(indata, shapeinfo, checkordered, name)

}

# check the shape of outdata is as expected. Does not recombine the task.
# should be called after recombinetask was called (but with the not-combined data)
# because that one gives info about type errors etc.
assertRetrafoOutput = function(outdata, datasplit, shapeinfo, name) {
  if (datasplit %in% c("all", "most")) {
    assertSetEqual(names(outdata), names(shapeinfo))
    for (n in names(outdata)) {
      assertShapeConform(outdata[[n]], shapeinfo[[n]], datasplit == "all", name)
    }
  }
  invisible(NULL)
}

# data can be a task or a data.frame
# description should be something like 'data going into / coming out of %NAME%'
checkDataConformsProperties(data, allowed.properties, description) {
  if (is.data.frame(data)) {
    td = makeTaskDescInternal(NULL, NULL, data, character(0), NULL, NULL)
  } else {
    assertClass(data, "Task")
    td = getTaskDesc(data)
  }
  nf = td$n.feat
  present.properties = c(names(nf)[nf > 0], if (td$has.missings) "missings")
  assertSubset(present.properties, allowed.properties, .var.name = description)
}

# do the preparation before calling trafo:
#  - check the data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - split the data
#  - get a shape info object
#  --> return list(indata = list(data, target), shapeinfo)
prepareTrafoInput = function(indata, datasplit, allowed.properties, name) {
  assert(checkClass(indata, "data.frame"), checkClass(indata, "Task"))

  checkDataConformsProperties(indata, allowed.properties, sprintf("Data going into %s trafo", name))

  shapeinfo = makeInputShapeinfo(indata)

  indata = if (is.data.frame(indata)) {
    splitdf(indata, datasplit)
  } else {
    splittask(indata, datasplit)
  }
  list(indata, shapeinfo)
}

# do the preparation before calling retrafo:
#  - check data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - split the data
#  --> return the data in a shape fit to be fed into retrafo
prepareRetrafoInput = function(indata, datasplit, allowed.properties, shapeinfo.input, name) {

  checkRetrafoInput(indata, datasplit == "all", shapeinfo.input, name)

  checkDataConformsProperties(indata, allowed.properties, sprintf("Data going into %s retrafo", name))

  if (datasplit %in% c("most", "all")) {
    splitinto = c("numeric", "factor", "other", if (datasplit == "all") "ordered")
    splitColsByType(splitinto, indata)
  } else {
    indata
  }
}

# do the check of the trafo's return value
#  - check the data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check properties are allowed
#  - get a shape info object
#  --> return list(outdata, shapeinfo)
handleTrafoOutput(outdata, olddata, datasplit, allowed.properties, name) {
  recombined = if (is.data.frame(olddata)) {
    recombinedf(olddata, outdata, datasplit, name)
  } else {
    recombinetask(olddata, outdata, datasplit, name)
  }
  checkDataConformsProperties(recombined, allowed.properties, sprintf("%s trafo return value", name))

  shapeinfo = makeOutputShapeInfo(olddata)

  list(outdata = recombined, shapeinfo = shapeinfo)
}

# do the check of the retrafo's return value
#  - check data is in an acceptable format (task, df, split dfs)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - recombine into a task / df
#  --> return the data in a shape fit to be fed into retrafo
handleRetrafoOutput = function(outdata, olddata, datasplit, allowed.properties, shapeinfo.output, name) {
  recombined = if (is.data.frame(olddata)) {
    recombinedf(olddata, outdata, datasplit, name)
  } else {
    recombinetask(olddata, outdata, datasplit, name)
  }

  checkDataConformsProperties(recombined, allowed.properties, sprintf("%s retrafo return value", name))

  assertRetrafoOutput(outdata, datasplit, shapeinfo.output, name)

  recombined
}




# test that:
#  changing some of the columns leaves the others in order
#  change of target gives error
#  duplicate introduced name gives error
#  new task is actually changed, has the expected data

# training with data.frame, predicting with task, etc.
#
