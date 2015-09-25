# check validity of param set. currently we only look at requires.
# FIXME: this should be done in ParamHelpers. Code should be removed soon. See PH#52
checkParamSet = function(ps) {
  for (p in ps$pars) {
    if (!is.null(p$requires) && is.expression(p$requires)) {
      stopf("Parameter '%s' in param set used 'expression' to define its 'requires'. You must use 'quote' instead!", p$id)
    }
  }
}
