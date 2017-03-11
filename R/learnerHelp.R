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
        n <- readline("0 : cancel\n...: ")
        n <- ifelse(grepl("\\D",n),-1,as.integer(n))
        if (is.finite(n) && n >= 1 && n <= length(learner$callees)) {
          break
        }
	if (identical(n, 0L)) {
          return(invisible(NULL))
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


