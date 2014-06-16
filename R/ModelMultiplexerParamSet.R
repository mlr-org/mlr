#' @title Creates a parameter set for model multiplexer tuning.
#'
#' @description
#' Handy way to create the param set with too much typing.
#'
#' The following is done automatically:
#' \itemize{
#' \item{The \code{selected.learner} param is created}
#' \item{Parameter names are prefixed.}
#' \item{The \code{requires} field of each param is set.
#'   This makes all parameters subordinate to \code{selected.learner}}
#' }
#'
#' @param multiplexer [\code{\link{ModelMultiplexer}}]\cr
#'   The muliplexer learner.
#' @param ... [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Named param sets. Names must correspond to base learners.
#'   You only need to enter the parameters you want to tune without reference
#'   to the \code{selected.learner} field in any way.
#' @param .check [\code{logical}]\cr
#'   Check that for each param in \code{...} one param in found in the base learners.
#'   Default is \code{TRUE}
#' @template ret_ps
#' @export
#' @family multiplexer, tune
#' @examples
#' # See makeModelMultiplexer
makeModelMultiplexerParamSet = function(multiplexer, ..., .check = TRUE) {
  checkArg(multiplexer, "ModelMultiplexer")
  checkArg(.check, "logical", len = 1L, na.ok = TRUE)

  bls = multiplexer$base.learners
  bl.ids = extractSubList(bls, "id")

  new.ps = makeParamSet(
    makeDiscreteParam("selected.learner", values = bl.ids)
  )

  pss = list(...)
  checkListElementClass(pss, "ParamSet")
  pss.ids = names(pss)

  # iterate thru params: prefix param id with base learner id and set requires field
  for (i in seq_along(pss)) {
    ps = pss[[i]]
    ps.id = pss.ids[i]
    bl = bls[[ps.id]]
    if (is.null(bl))
      stopf("Passed param set for '%s', no base learner in multiplexer with this id!", ps.id)
    for (j in seq_along(ps$pars)) {
      p = ps$pars[[j]]
      pid = p$id
      if (.check && (pid %nin% getParamIds(bl$par.set)))
        stopf("No param of id '%s' in base learner '%s'!", pid, bl$id)
      p$id = paste(bl$id, pid, sep = ".")
      p$requires = asQuoted(sprintf("selected.learner == '%s'", bl$id))
      ps$pars[[j]] = p
    }
    pss[[i]] = ps
  }

  new.ps = c(new.ps, do.call(c, pss))

  return(new.ps)
}



