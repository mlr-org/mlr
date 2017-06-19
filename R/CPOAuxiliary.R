#' @title CPO: Composable Preprocessing Operators
#'
#' @description
#' FIXME to come
#'
#' @family CPO
#' @name CPO
NULL

#' @title CPO composition neutral element
#'
#' @description
#' FIXME to come
#'
#' @family CPO
#' @export
NULLCPO = makeS3Obj(c("NULLCPO", "CPOPrimitive", "CPORetrafo", "CPO"))  # nolint

##################################
### %>>% Operator              ###
##################################

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

#' @title CPO Composition / Attachment operator
#'
#' @rdname grapes-greater-than-greater-than-grapes
#' @export
`%<<%` = function(cpo2, cpo1) {
  cpo1 %>>% cpo2
}

#' @export
`%>>%.default` = function(cpo1, cpo2) {
  stopf("%%>>%% not defined for objects of class c(%s)", paste0('"', class(cpo1), '"', collapse = ", "))
}

#' @export
`%>>%.data.frame` = function(cpo1, cpo2) {
  `%>>%.Task`(cpo1, cpo2)
}

#' @export
`%>>%.Task` = function(cpo1, cpo2) {
  if ("Learner" %in% class(cpo2)) {
    stopf("%s\n%s\n%s\n%s",
      "Cannot pipe data into learner!",
      "If you called 'data %>>% preproc %>>% learner', you probably meant",
      "train(preproc %>>% learner, data). Note that this is different from",
      "'train(learner, data %>>% preproc), which is usually not what you want.")
  } else if (any(c("CPORetrafo", "CPO") %in% class(cpo2))) {
    applyCPO(cpo2, cpo1)
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
`%>>%.NULLCPO` = function(cpo1, cpo2) {
  if (any(c("Learner", "CPO", "CPORetrafo") %in% class(cpo2))) {
    cpo2
  } else {
    NextMethod()
  }
}


#' @export
`%>>%.CPO` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPO" %in% class(cpo2)) {
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

#' @export
`%>>%.CPORetrafo` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPORetrafo" %in% class(cpo2)) {
    composeCPO(cpo1, cpo2)
  } else if ("WrappedModel" %in% class(cpo2)) {
    stop("Attaching CPO Retrafo to a model is not implemented.")
  } else if ("CPO" %in% class(cpo2) || "CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Retrafo with CPO.")
  } else {
    stopf("Cannot compose CPO Retrafo with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

##################################
### Generics                   ###
##################################


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
#'
#' @return [\code{CPORetrafo}]. The retransformation function that can be
#'   applied to new data.
#'
#' @examples
#' \dontrun{
#' # FIXME: need to update this
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
#'
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
retrafo = function(data) {
  UseMethod("retrafo")
}


#' @title Get the prediction inverse function
#'
#' @description
#' Gets the retrafo function that can be applied to the prediction.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The result of a \code{\link{\%>>\%}} chain applied to a data set.
#'
#' @family CPO
#' @export
inverter = function(data) {
  UseMethod("inverter")
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


#' @title Set the prediction inverse function
#'
#' @description
#' Sets the retrafo function that can be applied to the prediction.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   Something to be applied to a \code{\link{\%>>\%}} chain.
#'
#' @param value [\code{CPORetrafo}]\cr
#'   An inverter chain.
#'
#' @family CPO
#' @export
`inverter<-` = function(data, value) {
  UseMethod("inverter<-")
}


#' @title the ID of a CPO object.
#'
#' @description
#' Setting the ID of a CPO to a value will prefix all its
#' parameter names with this ID. This makes it possible to
#' compose CPOs that have clashing parameter names.
#'
#' @export
setCPOId = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  if (!"CPOPrimitive" %in% class(cpo)) {
    stop("Cannot set ID of compound CPO.")
  }
  UseMethod("setCPOId")
}

#' @title Get the ID of a CPO object.
#'
#' @description
#' The ID of a CPO to a value will prefix all its
#' parameter names with this ID.
#'
#' @family CPO
#' @export
getCPOId = function(cpo) {
  UseMethod("getCPOId")
}

#' @export
getCPOId.CPO = function(cpo) {
  stop("Compound CPOs have no IDs.")
}

#' @title Get the Selection Arguments for affected CPOs
#'
#' @description
#' Get the \code{affected.*} arguments from when the CPO was constructed.
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#' @param drop.defaults [\code{logical(1)}]\cr
#'   Whether to only return the arguments that deviate from the default.
#'   Default is \code{TRUE}.
#'
#' @family CPO
#' @export
getCPOAffect = function(cpo, drop.defaults = TRUE) {
  UseMethod("getCPOAffect")
}

#' @export
getCPOAffect.CPO = function(cpo, drop.defaults = TRUE) {
  stop("Compound CPOs have no affect arguments.")
}


#' @title Get the Properties of the given CPO object
#'
#' @description
#' The properties of a CPO object determine the kind of data the CPO will be able to handle, and how
#' it transforms data. Each entry can be one of: \dQuote{numerics}, \dQuote{factors}, \dQuote{ordered},
#' \dQuote{missings}.
#'
#' This function returns a list of three values: \dQuote{properties}, \dQuote{properties.adding}, and
#' \dQuote{properties.needed}.
#'
#' The \dQuote{properties} determines what data the CPO handles. If a property of a given data set is absent,
#' the preproc operator will reject the data.
#'
#' \dQuote{properties.adding} can be one or many of the same values as \dQuote{properties}. These properties
#' get added to a Learner (or CPO) coming after / behind this CPO. When a CPO imputes missing values, for example,
#' this is \dQuote{missings}. This is always a subset of \dQuote{properties}.
#'
#' \dQuote{properties.needed} can be one or many of the same values as \dQuote{properties}. These properties
#' are required from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#' numerics, this is \dQuote{numerics} (and \dQuote{properties.adding} is \dQuote{factors}).
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO to query.
#'
#' @param only.data [\code{logical(1)}]\cr
#'   Only get the CPO properties relevant for data (not target or task types). Default is \code{FALSE}.
#'
#' @export
getCPOProperties = function(cpo, only.data = FALSE) {
  assertFlag(only.data)
  UseMethod("getCPOProperties")
}


#' @title Get the CPO Kind
#'
#' @description
#' Is either "trafo", "retrafo", or "inverter"
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#'
#' @export
getCPOKind = function(cpo) {
  UseMethod("getCPOKind")
}


#' @title Get the CPO predict.type
#'
#' @description
#' Get the possible predict.types this CPO is able to handle.
#'
#' @param cpo [\code{CPO}]\cr
#'   The CPO.
#'
#' @export
getCPOPredictType = function(cpo) {
  UseMethod("getCPOPredictType")
}


##################################
### CPO-Learner Disassembly    ###
##################################

#' @title Get the CPO associated with a learner
#'
#' @description
#' Returns the (outermost) chain of CPOs that are part of a learner. This is useful to inspect the
#' preprocessing done by a learner object.
#'
#' If there are hidden CPOs (e.g. if \dQuote{learner} has CPOs, but is wrapped by a \code{TuneWrapper}),
#' this function can not retrieve these CPOs, but it will emit a warning if \dQuote{warn.buried} is \dQuote{TRUE}.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner to query
#' @param warn.buried [\code{logical(1)}]\cr
#'   Whether to warn about CPOs that could not be retrieved.
#' @export
getLearnerCPO = function(learner, warn.buried = TRUE) {
  checkLearner(learner)
  name = getLearnerName(learner)
  appending = TRUE
  result = NULLCPO
  repeat {
    if (!"CPOLearner" %in% class(learner)) {
      if (is.atomic(learner) || is.null(learner$next.learner)) {
        break
      }
      appending = FALSE
    } else {
      if (is.atomic(learner) || is.null(learner$next.learner)) {
        stop("Error: found learner with class CPOLearner but without $next.learner slot.")
      }
      if (appending) {
        result = result %>>% singleLearnerCPO(learner)
      } else {
        warningf("Learner %s had buried CPOs", name)
        break
      }
    }
    learner = learner$next.learner
  }
  result
}

#' @title Get the learner with the reachable CPOs removed
#'
#' @description
#' Get the bare Learner without the CPOs that were previously added.
#'
#' It is still possible for the result to be a wrapped learner, e.g. a
#' TuningWrapper wrapped learner. It is also possible that below the
#' tuning wrapper, there are more CPOs. These will not be removed.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner to strip.
#' @export
getLearnerBare = function(learner) {
  checkLearner(learner)
  while ("CPOLearner" %in% class(learner)) {
    if (is.atomic(learner) || is.null(learner$next.learner)) {
      stop("Error: found learner with class CPOLearner but without $next.learner slot.")
    }
    learner = learner$next.learner
  }
  learner
}

singleLearnerCPO = function(learner) {
  UseMethod("singleLearnerCPO")
}

#' @title determine the bound of a CPO or Retrafo
#'
#' @description
#' Gives the bound of a CPO or Retrafo. These can be
#' \dQuote{targetbound} for a CPO / Retrafo that
#' manipulates target columns, \dQuote{databound} for
#' a CPO / Retrafo that manipulates non-target columns.
#'
#' For a CPO that affects both, it is a \code{character(2)}
#' with both values; for one that affects neither, it is
#' \code{character(0)}.
#'
#' @param cpo [\code{CPO} | \code{CPORetrafo}]\cr
#'   The CPO or Retrafo to inspect.
#'
#' @export
getCPOBound = function(cpo) {
  UseMethod("getCPOBound")
}


##################################
### Retrafo                    ###
##################################

#' @export
retrafo.default = function(data) {
  res = attr(data, "retrafo")
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("data is not a Task or data.frame.\n%s\n%s",
      "are you sure you are applying 'retrafo' to the result",
      "of a %>>% transformation?")
  } else if (is.null(res)) {
    res = NULLCPO
  }
  res
}

#' @export
retrafo.WrappedModel = function(data) {
  NULLCPO
}


#' @export
retrafo.CPOModel = function(data) {
  # go through the chained model and see if there are wrapped models that
  # are not %>>%-chained (since the user probably wants to be warned about
  # that.
  recurseRetrafo = function(model, prev) {
    res = singleModelRetrafo(model, prev)
    next.model = model$learner.model$next.model
    if ("BaseWrapperModel" %in% class(next.model)) {
      if ("CPOModel" %in% class(next.model)) {
        return(recurseRetrafo(next.model, res))
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
    res
  }
  recurseRetrafo(data, NULL)
}

singleModelRetrafo = function(model, prev) {
  UseMethod("singleModelRetrafo")
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
  if (is.nullcpo(value)) {
    value = NULL
  }
  attr(data, "retrafo") = value
  data
}

#' @export
`retrafo<-.WrappedModel` = function(data, value) {
  stop("Cannot change retrafo of a model!")
}


##################################
### Inverter                   ###
##################################


#' @export
inverter.default = function(data) {
  res = attr(data, "inverter")
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("data is not a Task or data.frame.\n%s\n%s",
      "are you sure you are applying 'retrafo' to the result",
      "of a %>>% transformation?")
  } else if (is.null(res)) {
    res = NULLCPO
  }
  res
}


#' @export
`inverter<-.default` = function(data, value) {
  if (!is.null(value)) {
    assertClass(value, "CPOInverter")
  }
  if (!any(c("data.frame", "Task") %in% class(data))) {
    warningf("argument is neither a Task nor data.frame.\n%s\n%s",
      "are you sure you are applying it to the input or",
      "result of a %>>% transformation?")
  }
  if (is.nullcpo(value)) {
    value = NULL
  }
  attr(data, "inverter") = value
  data
}

#' @title Set the Inverter Tag
#'
#' @description
#' Tag the data that when it is sent through
#' a \code{\link{\%>>\%}} chain, the inverter
#' will be kept.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The data to tag
#'
#' @param set [\code{logical(1)}]\cr
#'   Whether to set the tag  or unset it. Default is TRUE.
#'
#' @family CPO
#' @export
tagInvert = function(data, set = TRUE) {
  if (!any(c("data.frame", "Task") %in% class(data))) {
    stop("data is not a Task or data.frame.")
  }
  assertFlag(set)
  if (set) {
    attr(data, "keep.inverter") = TRUE
  } else {
    attr(data, "keep.inverter") = NULL
  }
  data
}

#' @title Get the Inverter Tag
#'
#' @description
#' Check whether the data is tagged for inverter saving
#' when it is sent through a \code{\link{\%>>\%}} chain.
#'
#' @param data [\code{data.frame} | \code{\link{Task}}]\cr
#'   The result of a \code{\link{\%>>\%}} chain applied to a data set.
#'
#' @family CPO
#' @export
hasTagInvert = function(data) {
  if (!any(c("data.frame", "Task") %in% class(data))) {
    stop("data is not a Task or data.frame.")
  }
  identical(attr(data, "keep.inverter"), TRUE)
}

#' @title Invert Target Preprocessing
#'
#' @description
#' Invert the transformation, done on the target column(s)
#' of a data set, after prediction.
#'
#' Use either a retrafo object, or an inverter retrieved with
#' \code{\link{inverter}} from a data object that was fed through a retrafo
#' chain with \code{\link{tagInvert}} set to \code{TRUE}.
#'
#' If the retrafo object used had no target-bound transformations,
#' this is mostly a no-op, except that it possibly changes the task description
#' of the prediction.
#'
#' @param inverter [\code{CPORetrafo}]\cr
#'   The retrafo or inverter to apply
#' @param prediction [\code{\link{Prediction}} | \code{matrix} | \code{data.frame}]\cr
#'   The prediction to invert
#' @return A transformed \code{\link{Prediction}} if a prediction was given,
#'   or a \code{data.frame}. The 'truth' column(s) of the prediction will be dropped.
#'
#' @export
invert = function(inverter, prediction, predict.type = "response") {
  assertClass(inverter, "CPORetrafo")

  have.prediction = "Prediction" %in% class(prediction)
  if (have.prediction) {
    preddf = prediction$data
    probs = grepl("^prob(\\..*)$", names(preddf))
    if (length(probs)) {
      preddf = preddf[probs]
    } else if ("se" %in% names(preddf)) {
      preddf = preddf[c("response", "se")]
    } else {
      preddf = preddf$response
    }
    assert(is.data.frame(preddf))
  } else {
    preddf = prediction
  }
  preddf = sanitizePrediction(preddf)

  if (is.nullcpo(inverter) || length(inverter$predict.type) > 2) {  # predict.type is the identity
    cat("(Inversion was a no-op.)\n")
    # we check this here and not earlier because the user might rely on inverter()
    # to check his data for consistency
    prediction
  }

  inverted = invertCPO(inverter, preddf, predict.type)
  invdata = inverted$new.prediction
  assert(all(grepl("^se$|^(prob|response)(\\..*)?$", names(invdata))))
  if (is.null(inverted$new.td)) {
    assert("retrafo" %in% getCPOKind(inverter))  # only hybrid retrafos should return a NULL td

    outputtype = intersect(getCPOProperties(inverter)$properties, cpo.tasktypes)
    assert(length(outputtype) == 1)  # hybrid retrafos should always have one, otherwise it is a bug.

    tdconstructor = get(sprintf("make%sTaskDesc", stri_trans_totitle(outputtype)), mode = "function")

    tdname = "[CPO CONSTRUCTED]"

    inverted$new.td = switch(outputtype,
      classif = {
        levels = ifelse(predict.type == "prob", colnames(invdata), levels(invdata))
        makeClassifTaskDesc(tdname, data.frame(target = factor(character(0), levels = levels)), "target", NULL, NULL, levels[1])
      },
      cluster = makeClusterTaskDesc(tdname, data.frame(), NULL, NULL),
      regr = makeRegrTaskDesc(tdname, data.frame(target = numeric(0)), "target", NULL, NULL),
      multilabel = makeMultilabelTaskDesc(tdname, as.data.frame(invdata)[integer(0), ], colnames(invdata), NULL, NULL),
      surv = makeSurvTaskDesc(tdname, data.frame(target1 = numeric(0), target2 = numeric(0)), c("target1", "target2"), NULL, NULL, "rcens"),
      # assuming rcens since nothing else ever gets used.
      stop("unknown outputtype"))
  }
  if (have.prediction) {
    makePrediction(inverted$new.td, row.names = rownames(invdata), id = prediction$data$id,
      truth = inverted$new.truth, predict.type = predict.type, predict.threshold = NULL, y = invdata, time = prediction$time,
      error = prediction$error, dump = prediction$dump)
  } else {
    invdata
  }
}

sanitizePrediction = function(data) {
  if (is.data.frame(data)) {
    if (length(unique(sapply(data, function(x) class(x)[1]))) != 1) {
      stop("Prediction had columns of multiple modes.")
    }
    if (ncol(data) > 1) {
      data = as.matrix(data)
    } else {
      data = data[[1]]
    }
  }
  if (is.matrix(data) && ncol(data) == 1) {
    data = data[, 1, drop = TRUE]
  }
  if (is.logical(data) && !is.matrix(data)) {
    data = matrix(data, ncol = 1)
  }
  if (!is.logical(data) && !is.numeric(data) && !is.factor(data)) {
    stop("Data did not conform to any possible prediction: Was not numeric, factorial, or logical")
  }
  data
}

inferPredictionTypePossibilities = function(data) {
  # the canonical data layout, after sanitizePrediction
  # regr response: numeric vector
  # regr se: numeric 2-column matrix
  # cluster response: integer vector
  # cluster prob: numeric matrix. This could also be a 1-D matrix but will be returned as numeric vector
  # classif response: logical vector
  # classif prob: numeric matrix > 1 column, except for oneclass possibly (numeric vector)
  # surv response: numeric vector
  # surv prob: assuming a numeric matrix > 1 column, but doesn't seem to currently exist
  # multiclass response: logical matrix > 1 column
  # multiclass prob: matrix > 1 column

  data = sanitizePrediction(data)
  if (is.matrix(data)) {
    if (mode(data) == "logical") {
      return("multilabel")
    }
    return(c("cluster", "classif", "multilabel", "surv", if (ncol(data) == 2) "regr"))
  }

  if (is.factor(data)) {
    "classif"
  } else if (!is.numeric(data)) {
    stop("Data did not conform to any possible prediction: Was not numeric or factorial")
  } else {
    areWhole = function(x, tol = .Machine$double.eps^0.25)  all(abs(x - round(x)) < tol)
    c(if (areWhole(data)) "cluster", "surv", "regr")
  }
}

# if 'typepossibilities' has one element, this will also return one element EXCEPT FOR CLASSIF, CLUSTER
getPredResponseType = function(data, typepossibilities) {
  assertSubset(typepossibilities, cpo.tasktypes, empty.ok = FALSE)
  errout = function() stopf("Data did not conform to any of the possible prediction types %s", collapse(typepossibilities))
  data = sanitizePrediction(data)
  if (is.matrix(data)) {
    if (mode(data) == "logical") {
      if (!"multilabel" %in% typepossibilities) errout()
      return("response")
    }
    if (ncol(data) == 2) {
      if (identical(typepossibilities, "regr")) {
        return("se")
      }
      return(c("se", "prob"))
    }
    if ("regr" %in% typepossibilities) errout()
    return("prob")
  }

  if (is.factor(data)) {
    if (!"classif" %in% typepossibilities) errout()
    return("response")
  }
  if (!numeric(data)) errout()
  areWhole = function(x, tol = .Machine$double.eps^0.25)  all(abs(x - round(x)) < tol)
  if (!areWhole(data) && !"surv" %in% typepossibilities && !"regr" %in% typepossibilities) errout()
  c("response", if (any(c("classif", "cluster") %in% typepossibilities)) "prob")
}

# 'prediction' is whatever type the prediction usually has (depending on type). must return
# a list (new.prediction, new.td, new.truth)
#
# new.td & new.truth may be NULL if no target change occurred.
invertCPO = function(inverter, prediction, predict.type) {
  UseMethod("invertCPO")
}


##################################
### Chaining                   ###
##################################


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
  assert(checkList(pplist, types = "CPO"),
    checkList(pplist, types = "CPORetrafo"))
  Reduce(`%>>%`, c(list(NULLCPO), pplist))
}

##################################
### General Generic Functions  ###
##################################

#' @export
setHyperPars2.CPORetrafo = function(learner, par.vals = list()) {
  stopf("Cannot change parameter values of retrafo object\n%s\n%s\n",
    "To create a retrafo with a specific state use makeRetrafoFromState.",
    "Get the state of an existing retrafo using getRetrafoState.")
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
predict.CPORetrafo = function(object, data, ...) {
  assert(length(list(...)) == 0)
  applyCPO(object, data)
}

#' @export
predict.NULLCPO = function(object, data, ...) {
  assert(length(list(...)) == 0)
  data
}

#' @export
getRetrafoState.CPORetrafo = function(retrafo.object) {
  stop("Cannot get state of compound retrafo. Use as.list to get individual elements")
}

#' @export
getRetrafoState.NULLCPO = function(retrafo.object) {
  NULL
}

#' @export
getParamSet.CPORetrafo = function(x) {
  stop("Cannot get param set of compound retrafo. Use as.list to get individual elements")
}

#' @export
getParamSet.NULLCPO = function(x) {
  makeParamSet()
}

#' @export
getHyperPars.CPORetrafo = function(learner, for.fun = c("train", "predict", "both")) {
  stop("Cannot get parameters of compound retrafo. Use as.list to get individual elements")
}

#' @export
getHyperPars.NULLCPO = function(learner, for.fun = c("train", "predict", "both")) {
  setNames(list(), character(0))
}

#' @export
setCPOId.default = function(cpo, id) {
  stop("setCPOId for object not defined.")
}

#' @export
setCPOId.NULLCPO = function(cpo, id) {
  stop("Cannot set ID of NULLCPO.")
}

#' @export
getCPOId.NULLCPO = function(cpo) {
  NULL
}

#' @export
getCPOAffect.NULLCPO = function(cpo, drop.defaults = TRUE) {
  list()
}

#' @export
getCPOName.CPORetrafo = function(cpo) {
  paste(sapply(as.list(cpo), getCPOName), collapse = " => ")
}

#' @export
getCPOName.NULLCPO = function(cpo) {
  "NULLCPO"
}

#' @export
getCPOBound.NULLCPO = function(cpo) {
  character(0)
}

invertCPO.NULLCPO = function(inverter, prediction, predict.type) {
  list(new.prediction = prediction, new.td = NULL, new.truth = NULL)
}

#' @export
as.list.NULLCPO = function(x, ...) {
  list()
}

invertCPO.CPO = function(inverter, prediction, predict.type) {
  stop("Cannot invert prediction with a CPO object; need a CPORetrafo object.")
}

is.nullcpo = function(cpo) {  # nolint
  "NULLCPO" %in% class(cpo)
}

#' @export
getLearnerProperties.CPOLearner = function(learner) {
  # we could do this dynamically, always query the learner below.
  # then learner's properties could depend on its hyperparameters.
  # Whenever there is a conflict of properties (a cpo producing
  # missings when the learner below in its configuration happens to
  # not be able to handle missings) one could return 'empty' properties
  # -- the learner is not able to handle any data.
  #
  # One would ideally check whether some properties are fixed or depend
  # on parameters, and give an error on construction if there is a conflict
  # that will always be present.
  #
  # The much simpler and almost as good solution is to give maximal freedom
  # with properties and to just let the learner crash when something does not
  # work together as it should. This is bound to happen anyways
  # (e.g. because missings don't give info which kind of data is missing, and
  #  some CPO might be fine with missings in factors, but not with missings in
  #  numerics) unless one rewrites the whole properties-datatypes-hyperparameter
  # stuff in something like prolog.
  learner$properties
}

##################################
### Printing                   ###
##################################

#' @export
print.CPOConstructor = function(x, ...) {
  args = dropNamed(formals(x), environment(x)$reserved.params)
  argvals = sapply(args, function(y) if (identical(y, substitute())) "" else paste(" =", convertToShortString(y)))
  argstring = paste(names(args), argvals, collapse = ", ", sep = "")
  catf("<<CPO %s(%s)>>", getCPOName(x), argstring)
}

#' @export
print.CPO = function(x, ...) {
  pv = getHyperPars(x)
  argstring = paste(names(pv), sapply(pv, convertToShortString), sep = " = ", collapse = ", ")
  template = ifelse("CPOPrimitive" %in% class(x), "%s(%s)", "(%s)(%s)")
  catf(template, getCPOName(x), argstring, newline = FALSE)
  if (("CPOPrimitive" %in% class(x)) && length({affect = getCPOAffect(x)})) {
    catf(" [%s]", paste(names(affect), sapply(affect, convertToShortString), sep = " = ", collapse = ", "))
  } else {
    cat("\n")
  }
}

#' @export
print.DetailedCPO = function(x, ...) {
  chain = as.list(x)
  catf("Retrafo chain of %d elements:", length(chain))
  is.first = TRUE
  for (retrafo in chain) {
    if (!is.first) {
      cat("  ====>\n")
    }
    is.first = FALSE
    print(retrafo)
    cat("\n")
    print(getParamSet(retrafo))
  }
}

#' @export
summary.CPO = function(object, ...) {
  if (!"DetailedCPO" %in% class(object)) {
    class(object) = c(head(class(object), -1), "DetailedCPO", "CPO")
  }
  object
}

#' @export
print.CPORetrafo = function(x, ...) {
  first = TRUE
  kind = getCPOKind(x)
  pt = getCPOPredictType(x)
  if (length(pt) == 3) {
    assert("retrafo" %in% kind)
    kind = "retrafo"
  }
  catf("CPO %s chain", collapse(stri_trans_totitle(kind), sep = " / "), newline = FALSE)
  if ("inverter" %in% kind) {
    catf("(able to predict '%s')", collapse(pt, sep = "', '"))
  } else {
    cat("\n")
  }
  for (primitive in as.list(x)) {
    if (!first) {
      cat("=>")
    }
    first = FALSE
    pv = getHyperPars(primitive)
    argstring = paste(names(pv), sapply(pv, convertToShortString), sep = " = ", collapse = ", ")
    catf("[RETRAFO %s(%s)]", getCPOName(primitive), argstring, newline = FALSE)
  }
  cat("\n")
}

#' @export
print.NULLCPO = function(x, ...) {
  cat("NULLCPO\n")
}

##################################
### Auxiliaries                ###
##################################

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

# get the subset of par.vals described by par set.
# check furthermore that this subset is complete,
# i.e. all parameters that have no unfulfilled requirements are there
subsetParams = function(par.vals, par.set) {
  par.vals[intersect(names(par.vals), names(par.set$pars))]
}

checkAllParams = function(par.vals, par.set, name) {
  present = names(par.vals)

  # these parameters are either present or have fulfilled requirements
  needed = names(Filter(function(x) {
    x$id %in% names(par.vals) ||
          is.null(x$requires) || isTRUE(try(eval(x$requires, envir = par.vals), silent = TRUE))
  }, par.set$pars))

  missing.pars = setdiff(needed, present)
  if (length(missing.pars)) {
    plur = length(missing.pars) > 1
    stopf("Parameter%s %s of CPO %s %s missing\n%s", ifelse(plur, "s", ""),
      collapse(missing.pars, sep = ", "), name, ifelse(plur, "are", "is"),
      "Either give it during construction, or with setHyperPars.")
  }
}



# check that all parameters are feasible according to their limits
# 'infeasible' parameters according to requirements are allowed
checkParamsFeasible = function(par.set, par.vals) {
  # names(par.vals) must be a subset of names(par.set$pars)
  oobreaction = coalesce(getMlrOption("on.par.out.of.bounds"), "stop")
  par.vals = par.vals = convertNamesToItemsDVP(par.vals, par.set)
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


# convert between character vectors and lists for discrete vector params
convertNamesToItemsDVP = function(par.vals, par.set) {
  oobreaction = coalesce(getMlrOption("on.par.out.of.bounds"), "stop")
  reactionFn = switch(oobreaction, stop = stopf, warn = warningf, quiet = list)  # nolint
  for (n in names(par.set$pars)) {
    if (!n %in% names(par.vals)) {
      next
    }
    par = par.set$pars[[n]]
    if (par$type != "discretevector") {
      next
    }
    if (!all(sapply(names(par$values), function(nn) identical(par$values[[nn]], nn)))) {
      next
    }
    if (!is.character(par.vals[[n]])) {
      reactionFn("Discrete Vector Parameter %s requires character vector, not list, as values.", n)
    }

    badpars = setdiff(par.vals[[n]], names(par$values))
    if (length(badpars)) {
      reactionFn("Value%s '%s' do%s not occur in (names of) feasible values for parameter %s.",
        ifelse(length(badpars) > 1, "s", ""), collapse(badpars, sep = "', '"),
        ifelse(length(badpars) > 1, "es", ""), n)
      next
    }
    par.vals[[n]] = par$values[par.vals[[n]]]
  }
  par.vals
}

convertItemsToNamesDVP = function(par.vals, par.set) {
  for (n in names(par.set$pars)) {
    par = par.set$pars[[n]]
    if (par$type != "discretevector") {
      next
    }
    if (!all(sapply(names(par$values), function(nn) identical(par$values[[nn]], nn)))) {
      next
    }
    if (all(sapply(par.vals[[n]], is.character))) {
      par.vals[[n]] = as.character(par.vals[[n]])
    }
  }
  par.vals
}




# Manipulate requirement expressions: rename all variables from
# one name to another. This gets problematic when e.g. one variable
# is named 'c', because then c(1, 2, 3) gets broken. Therefore we
# go through the expressions and change only those requirements that
# are not function calls.
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

# Search for references to variables (not function) named in 'pattern'
# return TRUE if any were found, FALE otherwise
referencesNonfunctionNames = function(expr, pattern) {
  startfrom = 1
  if (is.call(expr)) {
    if (!is.recursive(expr)) {
      return(FALSE)
    }
    startfrom = 2
    if (is.recursive(expr[[1]])) {
      startfrom = 1
    } else if (length(expr) == 1) {
      return(FALSE)
    }
  }
  if (is.recursive(expr)) {
    for (idx in seq(startfrom, length(expr))) {
      if (referencesNonfunctionNames(expr[[idx]], pattern)) {
        return(TRUE)
      }
    }
  } else if (is.symbol(expr) && as.character(expr) %in% pattern) {
    return(TRUE)
  }
  return(FALSE)
}

# create a function with expressions 'expr' in the environment 'env'.
# the function gets the argument list 'required.arglist.
# if 'expr' is actually a function, we just check that it has at least all the
# arguments in 'required.arglist' (or that is has ellipses), that all
# arguments have the same default values as required.arglist, and that
# arguments of the function that are not 'required' have a default value so
# there won't be an error when the function gets called later.
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


registerCPO = function(cpo, category, subcategory = NULL, description) {
  name = deparse(substitute(cpo))
  if (!is.function(cpo)) {
    name = cpo$name
    cponame = cpo$cponame
  } else {
    cpo = cpo()
    assertClass(cpo, "CPO")
    cponame = getCPOName(cpo)
  }
  assertString(cponame)
  assertString(name)
  assertString(category)
  if (is.null(subcategory)) {
    subcategory = ""
  }
  assertString(subcategory)
  assertString(description)
  assign("CPOLIST", c(parent.env(environment())$CPOLIST,
    list(list(name = name, cponame = cponame, category = category,
      subcategory = subcategory, description = description))),
    envir = parent.env(environment()))
}

#' @title List all built-in CPOs
#'
#' @description
#' Return a \code{data.frame} with the columns \dQuote{name},
#' \dQuote{cponame}, \dQuote{category}, \dQuote{subcategory},
#' \dQuote{description}.
#'
#' @export
listCPO = function() {
  df = convertListOfRowsToDataFrame(parent.env(environment())$CPOLIST)
  df = df[order(paste(df$category, df$subcategory, df$name, sep = "$")), ]
  df$description = as.character(df$description)
  df
}


# TO-DO:
#- check shapeinfo when reattaching retrafos
#- bare model (through retrafo() = NULL)
#- feature subsetting: names, indices, grepl
# --> but only those which support that? --> but only ignoring properties then?
# --> how about as a further datasplit-kind of property. If given, properties.adding must be 0, properties must be maximal.
# --> how about as an added parameter and automatic wrapping. much simpler, and setHyperPars friendly.
# --> how about subsetting is possible, but invalidates properties.adding

# test todo
# convertNamesToItems, ItemsToNames
# on.par.out.of.bounds setting
