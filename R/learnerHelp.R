#' @title Access help page of learner functions.
#'
#' @description Interactive function that gives the user quick access to the
#'   help pages associated with various functions involved in the given learner.
#' @template arg_learner
#' @export
#' @family learner
#' @family help
learnerHelp = function(learner) {
  learner = checkLearner(learner)
  callees = learner$callees
  if (identical(callees, "")) {
    callees = NULL
  }
  mlr.help = utils::help(learner$id, package = "mlr")
  if (length(mlr.help) > 0) {
    # we can give some more help
    callees = c("mlr specifics", callees)
  }
  if (!is.null(callees)) {
    n = 1
    if (length(callees) > 1) {
      repeat {
        cat("Choose help page:\n")
        cat(paste0(seq_along(callees), " : ", callees, "\n", collapse=""))
        n <- readline("0 : cancel\n...: ")
        n <- ifelse(grepl("\\D",n),-1,as.integer(n))
        if (is.finite(n) && n >= 1 && n <= length(callees)) {
          break
        }
	if (identical(n, 0L)) {
          return(invisible(NULL))
        }
        catf("Invalid input. Enter a number between 1 and %d", length(callees))
      }
    }
    if (length(mlr.help) > 0) {
      if (n == 1) {
        return(mlr.help)
      }
      n = n - 1
    }
    for (pkg_ref in stri_replace_all(learner$package, "", regex = "[+!_]")) {
      h = utils::help(callees[n], package = (pkg_ref))
      if (length(h) > 0) {
        return(h)
      }
    }
  }
  catf("No information about learner %s found.", coalesce(learner$name, learner$shortname, learner$id))
  invisible(NULL)
}

#' @title Get specific help for a learner's parameters.
#'
#' @description Print the description of parameters of a given learner. The description
#'   is automatically extracted from the help pages of the learner, so it may be incomplete.
#'
#' @template arg_learner
#' @param param [\code{character}]\cr
#'   Parameter(s) to describe. If no argument is given, all parameters with a description
#'   are printed.
#' @export
#' @family learner
#' @family help
learnerParamHelp = function(learner, param) {
  learner = checkLearner(learner)
  if (missing(param)) {
    param = names(learner$help.list)
  }
  all.param = getParamIds(learner$par.set)
  not.found.param = param[!param %in% all.param]
  if (length(not.found.param) > 0) {
    stopf("Parameters not found: %s", collapse(not.found.param, sep = ", "))
  }
  # put param in the same order as par.set
  param = all.param[all.param %in% param]
  for (p in param) {
    catf("  *%s*:", p)
    if (p %in% names(learner$help.list)) {
      cat(learner$help.list[[p]], "\n\n")
    } else {
      catf("No documentation found. Try to consult learnerHelp(%s).\n")
    }
  }
}

makeParamHelpList = function(funs, pkgs, par.set) {
  help.list = list()
  par.ids = getParamIds(par.set)
  pkgs = stri_replace_all(pkgs, "", regex = "[+!_]")
  for (f in rev(funs)) {
    for (pkg_ref in pkgs) {
      h = utils::help(f, package = (pkg_ref))
      if (length(h) > 0) {
        break
      }
    }
    if (length(h) == 0) {
      # catf("nothing found: %s", f)
      next
    }
    ghf = get(".getHelpFile", mode="function", envir=getNamespace("utils"))
    html = capture.output(tools::Rd2HTML(ghf(h)))
    html = XML::htmlParse(html)
    tab = XML::getNodeSet(html, "//table[@summary='R argblock']")
    if (length(tab) < 1) {
      next
    }
    tbl = XML::readHTMLTable(tab[[1]], header = FALSE, stringsAsFactors = FALSE)
    if (!identical(ncol(tbl), 2L)) {
      # bad formatting
      # catf("bad formatting: %s", f)
      next
    }
    for (row in seq_len(nrow(tbl))) {
      par.name = tbl[row, 1]
      if (par.name %in% par.ids) {
        help.list[[par.name]] = tbl[row, 2]
      } else {
        # catf("not interesting: %s par %s", f, par.name)
      }
    }
  }
  return(help.list)
}

# helper function to get learner's undocumented functions.
listUndocumentedPars = function(learner) {
  learner = checkLearner(learner)
  setdiff(getParamIds(learner$par.set), names(learner$help.list))
}

