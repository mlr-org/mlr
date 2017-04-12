# my.c.ParamSet = function (..., recursive = FALSE)
# {
#   pss = list(...)
#   pars = Reduce(c, lapply(pss, function(ps) ps$pars))
#   names(pars) = NULL
#   return(do.call(my.makeParamSet, pars))
# }
#
# my.makeParamSet = function (..., params = NULL, forbidden = NULL, keys = NULL)
# {
#   pars = list(...)
#   if (length(pars) > 0 && !is.null(params))
#     stop("You can only use one of ... or params!")
#   if (!is.null(params)) {
#     assertList(params, types = "Param")
#     pars = params
#   }
#   else {
#     assertList(pars, types = "Param")
#   }
#   ns = extractSubList(pars, "id")
#   if (anyDuplicated(ns))
#     stop("All parameters must have unique names!")
#   names(pars) = ns
#   par.set = makeS3Obj("ParamSet", pars = pars, forbidden = forbidden)
#   if (length(pars) > 0L) {
#     if (all(vlapply(pars, inherits, what = "LearnerParam"))) {
#       par.set = addClasses(par.set, classes = "LearnerParamSet")
#       keys = union(keys, c(ns, "task", "n", "p", "k", "type"))
#     }
#     if (!is.null(keys) && (hasExpression(par.set)))
#       ParamHelpers:::checkExpressionFeasibility(par.set = par.set, keys = keys)
#   }
#   return(par.set)
# }
