# FIXME
#makeRLearner.regr.sg.libsvm = function() {
#  makeRLearnerRegr(
#    cl = "regr.sg.libsvm",
#    package = "sg",
#    missings = FALSE,
#    numerics = TRUE,
#    factors = FALSE,
#    se = FALSE,
#    weights = FALSE
#  )
#}
#
#trainLearner.regr.sg.libsvm = function(.learner, .task, .subset,  ...) {
#  size_cache = 100
#  d = getTaskData(.task, .subset, target.extra=TRUE, class.as="-1+1")
#  # shogun wants features in as column vectors
#  train = t(d(as.matrix(d$data)))
#  pars = list(...)
#  sg('set_features', 'TRAIN', train)
#  sg('set_labels', 'TRAIN', y)
#  sg('new_regression', pars$type)
#  sg.setHyperPars(pars)
#  sg('train_regression')
#  svm = sg('get_svm')
#  # todo: saving traindat is very inefficient....
#  names(svm) = c("bias", "alphas")
#  list(svm=svm, control=pars, traindat=train, y=y)
#}
#
#
#sg.setHyperPars = function(control) {
#  sg('set_kernel', 'GAUSSIAN', 'REAL', control$size_cache, control$width)
#  sg('svr_tube_epsilon', control$epsilon)
#}
#
#predictLearner.regr.sg.libsvm = function(.learner, .model, .newdata, ...) {
#  # shogun wants features in as column vectors
#  .newdata = t(as.matrix(.newdata))
#  m = .model$learner.model
#  sg('set_features', 'TRAIN', m$traindat)
#  sg('set_labels', 'TRAIN', m$y)
#  sg('set_features', 'TEST', .newdata)
#  sg('set_svm', m$svm$bias, m$svm$alphas)
#  ctrl = m$control
#  sg.setHyperPars(ctrl)
#  sg('classify')
#}