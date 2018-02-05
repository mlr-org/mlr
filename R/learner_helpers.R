convertFamilyParamRegr = function(fam, family.nuirange, family.d) {
  switch(family,
    Gaussian = mboost::Gaussian(),
    Laplace = mboost::Laplace(),
    Huber = mboost::Huber(family.d),
    Poisson = mboost::Poisson(),
    GammaReg = mboost::GammaReg(nuirange = family.nuirange),
    NBinomial = mboost::NBinomial(nuirange = family.nuirange),
    Hurdle = mboost::Hurdle(nuirange = family.nuirange),
    custom.family = custom.family.definition,
    stopf("Unsupported family  value: %s,", fam)
  )
}

callLearnerWithOptionalWeights = function(lfun, form, data, ..., .task, .weights) {
  if (is.null(weights))
    lfun(form, data = getTaskData(.task, .subset), ...)
  else
    lfun(form, data = getTaskData(.task, .subset), weights = .weights, ...)
}
