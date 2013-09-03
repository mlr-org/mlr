# Infill criteria.
# CONVENTION: INFILL CRITERIA ARE ALWAYS MINIMIZED. SO A FEW BELOW ARE NEGATED VERSIONS!
#FIXME think about which criterias are for determinitic, which are for noisy case
# below is just guessed this...

# General interface
# 
# @param points [\code{data.frame}]\cr 
#   Points where to evaluate.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param design [\code{data.frame}]\cr 
#   Design of already visited points.
# @return [\code{numeric}]. Criterion values at \code{points}.

# mean response of model
# useful for deterministic and noisy
infillCritMeanResponse = function(points, model, control, par.set, design) {
  ifelse(control$minimize, 1, -1) * predict(model, newdata=points)$data$response
}

# model uncertainty 
# on its own not really useful for anything I suppose...
infillCritStandardError = function(points, model, control, par.set, design) {
  -predict(model, newdata=points)$data$se
}


# expected improvement
# useful for deterministic
infillCritEI = function(points, model, control, par.set, design) {
  maximize.mult = ifelse(control$minimize, 1, -1) 
  y = maximize.mult * design[, control$y.name]
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response 
  p.se = p$se
  y.min = min(y)
  d = y.min - p.mu
  xcr = d / p.se
  #FIXME: what is done in DiceOption::EI here for numerical reasons?
  #if (kriging.sd/sqrt(model@covariance@sd2) < 1e-06) {
  #  res = 0
  #  xcr = xcr.prob = xcr.dens = NULL
  #
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  ei = d * xcr.prob + p.se * xcr.dens
  return(-ei)
}


# lower confidence bound
# useful for deterministic
infillCritLCB = function(points, model, control, par.set, design) {
  maximize.mult = ifelse(control$minimize, 1, -1) 
  p = predict(model, newdata = points)$data
  lcb = maximize.mult * (p$response - control$infill.crit.lcb.lambda * p$se)
  return(lcb)
}



# augmented expected improvement, as designed by huang
# useful for noisy
infillCritAEI = function(points, model, control, par.set, design) {
  #FIXME: generalize new.noise.var for all models
  
  maximize.mult = ifelse(control$minimize, 1, -1) 
  y = maximize.mult * design[, control$y.name]
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response 
  p.se = p$se
  # FIXME: add this a constant in control 
  qk = p.mu + qnorm(0.75) * p.se
  y.min = p.mu[rank(qk, ties.method="random") == 1]
  d = y.min - p.mu
  xcr = d / p.se
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)

  new.noise.var = model$learner.model@covariance@nugget

  #if (sk < sqrt(model@covariance@sd2)/1e+06) {
  #FIXME: What actually happens here. Find out in DiceOptim
  aei = ifelse(p.se < 1e-06, 0, 
    (d * xcr.prob + p.se * xcr.dens) * (1 - sqrt(new.noise.var) / sqrt(new.noise.var + p.se^2)))
  return(-aei)
}

# infillCritAKG = function(points, model, ctrl=NULL) {
#   if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
#   apply(des, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var)
# }
# 
# infillCritAEIold = function(points, model, ctrl=NULL) {
#   if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
#   if(is.null(ctrl$y.min)) ctrl$y.min=NULL
#   apply(points, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var, y.min=ctrl$y.min)
# }  
#   