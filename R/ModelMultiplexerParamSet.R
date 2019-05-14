#' @title Creates a parameter set for model multiplexer tuning.
#'
#' @description
#' Handy way to create the param set with less typing.
#'
#' The following is done automatically:
#' \itemize{
#' \item{The `selected.learner` param is created}
#' \item{Parameter names are prefixed.}
#' \item{The `requires` field of each param is set.
#'   This makes all parameters subordinate to `selected.learner`}
#' }
#'
#' @param multiplexer ([ModelMultiplexer])\cr
#'   The muliplexer learner.
#' @param ... ([ParamHelpers::ParamSet] | [ParamHelpers::Param])\cr
#'   (a) First option: Named param sets. Names must correspond to base learners.
#'   You only need to enter the parameters you want to tune without reference
#'   to the `selected.learner` field in any way.\cr
#'   (b) Second option. Just the params you would enter in the param sets.
#'   Even shorter to create. Only works when it can be uniquely identified to which
#'   learner each of your passed parameters belongs.
#' @param .check ([logical])\cr
#'   Check that for each param in `...` one param in found in the base learners.
#'   Default is `TRUE`
#' @template ret_ps
#' @export
#' @family multiplexer
#' @family tune
#' @examples
#' # See makeModelMultiplexer
makeModelMultiplexerParamSet = function(multiplexer, ..., .check = TRUE) {

  assertClass(multiplexer, classes = "ModelMultiplexer")
  assertFlag(.check)

  bls = multiplexer$base.learners
  bl.ids = extractSubList(bls, "id")

  args = list(...)
  new.ps = makeParamSet(
    makeDiscreteParam("selected.learner", values = bl.ids)
  )
  if (length(args) == 0L) {
    return(new.ps)
  }

  # if basic param were passed we now group them into param sets
  # we match each param in the base learners and add it to the correct parset
  if (inherits(args[[1L]], "Param")) {
    checkListElementClass(args, "Param")
    # our target result + all available params
    pss = namedList(bl.ids, makeParamSet())
    all.par.ids = getParamIds(multiplexer$par.set)

    for (j in seq_along(args)) {
      p = args[[j]]
      pid = p$id
      # end of param name we need to find
      long.pid.end = sprintf("\\.%s$", pid)
      found = stri_subset_regex(all.par.ids, long.pid.end)
      if (length(found) == 0L) {
        stopf("No param of id '%s' in any base learner!", pid)
      }
      if (length(found) > 1L) {
        stopf("Multiple params of id '%s' found in base learners, pass correctly grouped param sets!", pid)
      }
      # get the learner that is referenced from prefix of found string + add param to correct parset
      for.learner = stri_replace(found, "", regex = long.pid.end)
      for.pars = pss[[for.learner]]$pars
      for.pars[[pid]] = p
      pss[[for.learner]]$pars = for.pars
    }
    # remove empty stuff we did not fill + add to new.ps
    pss = Filter(Negate(isEmpty), pss)
  } else {
    checkListElementClass(args, "ParamSet")
    pss = args
  }

  pss.ids = names(pss)

  # iterate thru params: prefix param id with base learner id and set requires field
  for (i in seq_along(pss)) {
    ps = pss[[i]]
    ps.id = pss.ids[i]
    bl = bls[[ps.id]]
    if (is.null(bl)) {
      stopf("Passed param set for '%s', no base learner in multiplexer with this id!", ps.id)
    }
    for (j in seq_along(ps$pars)) {
      p = ps$pars[[j]]
      pid = p$id
      if (.check && (pid %nin% getParamIds(bl$par.set))) {
        stopf("No param of id '%s' in base learner '%s'!", pid, bl$id)
      }
      p$id = stri_paste(bl$id, pid, sep = ".")
      p$requires = asQuoted(sprintf("selected.learner == '%s'", bl$id))
      ps$pars[[j]] = p
    }
    pss[[i]] = ps
  }

  new.ps = c(new.ps, do.call(c, pss))

  return(new.ps)
}
