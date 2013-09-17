# Optimizers for multipoint infill criteria

# General interface
#
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @return [\code{data.frame}]. Proposed points that should be evaluated.

# Use LCB single crit but sample multiple different lambdas
multipointInfillOptLCB = function(model, control, par.set, opt.path, design) {
  # copy control and optimize multiple times with singlecrit lcb / different lambda
  control2 = control
  control2$propose.points = 1
  control2$infill.crit = "lcb"
  newdes = data.frame()
  lambdas = c()
  #FIXME could be done in parallel
  while (nrow(newdes) < control$propose.points) {
    # draw lambda from exp dist
    control2$infill.crit.lcb.lambda = rexp(1)
    newdes1 = proposePoints(model, par.set, control2, opt.path)
    # as we might construct the same xs for similar lamba, we
    # require that a new point is not nearly the same as another proposed one
    if (nrow(newdes) > 0L) {
      # FIXME what do we here for factor vars, wrt dist?
      dists = apply(newdes, 1, function(x) sum((x - newdes1[1,])^2))
    } else {
      dists = Inf
    }
    #FIXME how do we set this min value?
    if (min(dists) > 1e-5) {
      newdes = rbind(newdes, newdes1)
      lambdas = c(lambdas, control2$infill.crit.lcb.lambda)
    }
  }
  setAttribute(newdes, "multipoint.lcb.lambdas", lambdas)
}
