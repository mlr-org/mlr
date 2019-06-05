#' @title Access help page of learner functions.
#'
#' @description Interactive function that gives the user quick access to the
#'   help pages associated with various functions involved in the given learner.
#' @template arg_learner
#' @export
#' @family learner
#' @family help
helpLearner = function(learner) {
  learner = checkLearner(learner)
  callees = learner$callees
  if (identical(callees, "")) {
    callees = character(0L)
  }
  mlr.help = utils::help(learner$id, package = "mlr")
  if (length(mlr.help) > 0) {
    # we can give some more help
    callees = c("mlr specifics", callees)
  }
  if (length(callees) > 0) {
    n = 1
    if (length(callees) > 1) {
      repeat {
        cat("Choose help page:\n")
        cat(stri_join(seq_along(callees), " : ", callees, "\n", collapse = ""))
        n = readline("0 : cancel\n...: ")
        n = suppressWarnings(as.integer(n))
        if (is.finite(n) && n >= 1 && n <= length(callees)) {
          break
        }
        if (identical(n, 0L)) {
          return(invisible(NULL))
        }
        catf("Invalid input. Enter a number between 0 and %d", length(callees))
      }
    }
    if (length(mlr.help) > 0 && n == 1) {
      return(mlr.help)
    }
    for (pkg_ref in stri_replace_all(learner$package, "", regex = "[+!_]")) {
      h = utils::help(callees[n], package = (pkg_ref))
      if (length(h) > 0) {
        return(h)
      }
    }
  }
  catf("No information about learner %s found.",
    coalesce(learner$name, learner$shortname, learner$id))
  invisible(NULL)
}

#' @title Get specific help for a learner's parameters.
#'
#' @description Print the description of parameters of a given learner. The description
#'   is automatically extracted from the help pages of the learner, so it may be incomplete.
#'
#' @template arg_learner
#' @param param (`character` | NULL)\cr
#'   Parameter(s) to describe. Defaults to NULL, which prints information on the documentation
#'   status of all parameters.
#' @export
#' @family learner
#' @family help
helpLearnerParam = function(learner, param = NULL) {

  learner = checkLearner(learner)
  if (!inherits(learner, "RLearner")) {
    current.learner = learner
    repeat {
      next.learner = current.learner$next.learner
      if (is.null(next.learner)) {
        break
      }
      if (inherits(next.learner, "RLearner")) {
        messagef("The learner '%s' is a wrapped learner. Showing documentation of '%s' instead.",
          learner$id, next.learner$id)
        return(Recall(next.learner, param))
      }
      current.learner = next.learner
    }
    stop("The learner does not provide documentation.")
  }
  if (is.null(param)) {
    if (length(learner$help.list) > 0) {
      cat("Documentation for the following parameters is present:\n")
      for (p in names(learner$help.list)) {
        catf("- %s (%s)", p,
          stri_match_first_regex(learner$help.list[[p]], "(?<= )[^ ]*::[^\\n]*"))
      }
    }
    missingdoc = setdiff(getParamIds(learner$par.set), names(learner$help.list))
    if (length(missingdoc) > 0) {
      catf("The following parameters are missing documentation:\n%s",
        collapse(strwrap(collapse(missingdoc, sep = ", "), indent = 2), "\n"))
    }
    return(invisible(NULL))
  }
  assertCharacter(param, any.missing = FALSE)
  all.param = getParamIds(learner$par.set)
  not.found.param = param[!param %in% all.param]
  if (length(not.found.param) > 0) {
    stopf("Parameters not found: %s", collapse(not.found.param, sep = ", "))
  }
  # put param in the same order as par.set
  param = all.param[all.param %in% param]
  values = getHyperPars(learner)
  for (p in param) {
    catf("  *%s*:", p)
    print(learner$par.set$pars[[p]])
    req = learner$par.set$pars[[p]]$requires
    if (!is.null(req)) {
      cat("Requires: ")
      print(req)
    }
    if (!is.null(values[[p]])) {
      catf("Value: %s",
        ifelse(testScalar(values[[p]]), values[[p]], capture.output(print(values[[p]]))))
    }
    if (p %in% names(learner$help.list)) {
      output = learner$help.list[[p]]
      cat(collapse(strwrap(output), sep = "\n"), "\n\n")
    } else {
      catf("No documentation found. Try to consult helpLearner(\"%s\").\n", getLearnerId(learner))
    }
  }
}

# remove nesting levels of XML tags
simplifyNode = function(node) {
  children = XML::xmlChildren(node)
  lens = nchar(stri_trim(vcapply(children, XML::xmlValue)))
  if (length(lens) < 1) {
    return(NULL)
  }
  if (sum(lens) == max(lens)) {
    return(simplifyNode(children[[which.max(lens)]]))
  } else {
    children = children[lens != 0]
    firstchild = simplifyNode(children[[1]])
    if (length(firstchild) > 0) {
      children = c(firstchild, children[-1])
    }
    return(children)
  }
}

# collect all <li><code>xxx</code>yyy</li> in the document
# and form a data.frame with two columns corresponding to xxx and yyy.
codeListToTable = function(html) {
  lis = XML::getNodeSet(html, "//li")
  lislis = lapply(lis, function(li) {
    lichi = simplifyNode(li)
    if (length(lichi) < 2 || names(lichi)[1] != "code") {
      return(NULL)
    }
    parname = XML::xmlValue(lichi[[1]])
    pardesc = stri_join_list(lapply(lichi[-1], XML::xmlValue), collapse = " ")
    stri_trim(c(parname, pardesc), pattern = c("[a-zA-Z0-9_.]", "\\P{Wspace}"))
  })
  as.data.frame(do.call(rbind, lislis), stringsAsFactors = FALSE)
}

# Remove superfluous newlines.
prepareString = function(string) {
  # turn 'a  \n   \n  \n b' into 'a\n\nb'
  string = stri_replace_all(string, "\n\n", regex = " *\n *(\n *)+")
  # turn 'a \n b' into 'a b'
  string = stri_replace_all(string, " ", regex = "(?<!\n) *\n *(?!\n)")
  # turn ' \n\n ' into '\n'
  # strwrap does this for us, apparently.
  # string = stri_replace_all(string, "\n", regex = " *\n\n *")
  return(string)
}

makeParamHelpList = function(funs, pkgs, par.set) {
  help.list = list()
  par.ids = getParamIds(par.set)
  pkgs = stri_replace_all(pkgs, "", regex = "[+!_]")
  h = NULL
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
    ghf = get(".getHelpFile", mode = "function", envir = getNamespace("utils"))
    html = capture.output(tools::Rd2HTML(ghf(h)))
    html = XML::htmlParse(html)
    # try to extract the 'R argblock' table
    tab = XML::getNodeSet(html, "//table[@summary='R argblock']")
    if (length(tab) < 1) {
      next
    }
    tbl = do.call(rbind, lapply(tab, function(t) {
      tbl = XML::readHTMLTable(t, header = FALSE, stringsAsFactors = FALSE)
      if (identical(ncol(tbl), 2L)) {
        tbl
      }
    }))
    # we also try to extract generally all lists that begin with a code string.
    # this gets appended to the front, so that the (more trustworthy) html table
    # extract will overwrite the <li>-extract if a param occurs in both.
    tbl = rbind(codeListToTable(html), tbl)
    if (is.null(tbl)) {
      next
    }
    for (row in seq_len(nrow(tbl))) {
      # the following loop handles occasions where multiple parameters are mentioned in
      # one row, separated by commas.
      for (par.name in stri_split(tbl[row, 1], regex = ", *")[[1]]) {
        if (par.name %in% par.ids) {
          help.list[[par.name]] = stri_join("Argument of: ",
            pkg_ref, "::", f, "\n\n", prepareString(tbl[row, 2]))
        } else {
          # catf("not interesting: %s par %s", f, par.name)
        }
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
