#' @title Get the type of the learner.
#'
#' @description Get the type of the learner.
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerType = function(learner) {
  learner = checkLearner(learner)
  return(learner$type)
}

#' @title Get the ID of the learner.
#'
#' @description Get the ID of the learner.
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerId = function(learner) {
  learner = checkLearner(learner)
  return(learner$id)
}

#' @title Get the predict type of the learner.
#'
#' @description Get the predict type of the learner.
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerPredictType = function(learner) {
  learner = checkLearner(learner)
  return(learner$predict.type)
}

#' @title Get the required R packages of the learner.
#'
#' @description Get the R packages the learner requires.
#' @template arg_learner
#' @return [\code{character}].
#' @export
#' @family learner
getLearnerPackages = function(learner) {
  learner = checkLearner(learner)
  return(learner$package)
}


#' @title Get the parameter set of the learner.
#'
#' @description
#' Alias for \code{\link{getParamSet}}.
#'
#' @template arg_learner
#' @template ret_ps
#' @export
#' @family learner
getLearnerParamSet = function(learner) {
  getParamSet(learner)
}


#' @title Get the parameter values of the learner.
#'
#' @description
#' Alias for \code{\link{getHyperPars}}.
#'
#' @template arg_learner
#' @inheritParams getHyperPars
#' @return [\code{list}]. A named list of values.
#' @export
#' @family learner
getLearnerParVals = function(learner, for.fun = c("train", "predict", "both")) {
  learner = checkLearner(learner)
  getHyperPars(learner, for.fun)
}

#' @title Set the ID of a learner object.
#'
#' @description Set the ID of the learner.
#' @template arg_learner
#' @param id [\code{character(1)}]\cr
#'    New ID for learner.
#' @template ret_learner
#' @export
#' @family learner
setLearnerId = function(learner, id) {
  learner = checkLearner(learner)
  assertString(id)
  learner$id = id
  return(learner)
}

#' @title Get the short name of the learner.
#'
#' @description For an ordinary learner simply its short name is returned.
#'   For wrapped learners, the wrapper id is successively attached to the short
#'   name of the base learner. E.g: \dQuote{rf.bagged.imputed}
#' @template arg_learner
#' @return [\code{character(1)}].
#' @export
#' @family learner
getLearnerShortName = function(learner) {
  learner = checkLearner(learner)
  learner.short.name = learner$short.name

  if (is.null(learner.short.name)) {
    learner.id.split = unlist(strsplit(getLearnerId(learner), "[.]"))
    wrapper.ids = learner.id.split[3:length(learner.id.split)]
    base.learner.path = c(rep("next.learner", length(wrapper.ids)),
      "short.name")
    base.learner.short.name = extractSubList(list(learner),
      base.learner.path)
    learner.short.name = paste(c(base.learner.short.name, wrapper.ids),
      collapse = ".")
  }

  return(learner.short.name)
}


#' @title Access help page of learner functions.
#'
#' @description Interactive function that gives the user quick access to the
#'   help pages associated with various functions involved in the given learner.
#' @template arg_learner
#' @export
#' @family learner
learnerHelp = function(learner) {
  learner = checkLearner(learner)
  if (!is.null(learner$callees) && learner$callees[1] != "") {
    n = 1
    if (length(learner$callees) > 1) {
      repeat {
        cat("Choose help page:\n")
        cat(paste0(seq_along(learner$callees), " : ", learner$callees, "\n", collapse=""))
        n <- readline("(Ctrl-C to cancel)\n...: ")
        n <- ifelse(grepl("\\D",n),-1,as.integer(n))
        if (is.finite(n) && n >= 1 && n <= length(learner$callees)) {
          break
        }
        catf("Invalid input. Enter a number between 1 and %d", length(learner$callees))
      }
    }
    for (pkg_ref in learner$package) {
      h = utils::help(learner$callees[n], package = (pkg_ref))
      if (length(h) > 0) {
        return(h)
      }
    }
  }
  catf("No information about learner %s found.", coalesce(learner$name, learner$shortname, learner$id))
  invisible(NULL)
}
