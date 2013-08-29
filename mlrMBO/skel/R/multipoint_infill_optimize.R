# Optimizers for multipoint infill criteria

# General interface
#
# @param infill.crit [\code{function}]\cr
#   Infill criterion function.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @return [\code{data.frame}]. One proposed point that should be evaluated.

# mean response of model
multipointInfillOptRandom = function(model, control, par.set, opt.path, design) {
  print(control)
  newdes = generateDesign(control$n.propose.points, par.set, randomLHS, ints.as.num=TRUE)
}


# mean response of model
multipointInfillOptRandom = function(model, control, par.set, opt.path, design) {
  objfun = 
  print(control)
  newdes = generateDesign(control$n.propose.points, par.set, randomLHS, ints.as.num=TRUE)
}
