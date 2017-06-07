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
    assertFunction(value, nargs = 1)
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

# TO-DO:
#- getControl for retrafo
#- fromControl: create from control
#  setControl

#- taskstyle: whether to get the task, data as whole df, data sep from target, data sep into types
#- task disassemble / assemble methods
#- column names
