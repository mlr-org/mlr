#makeRLearner.regr.logicreg = function() {
#  makeRLearnerRegr(
#    cl = "regr.logicreg",
#    package = "LogicReg",
#    par.set = makeParamSet(
#      makeIntegerLearnerParam(id="ntrees", lower=1L, upper=5L), 
#      makeIntegerLearnerParam(id="nleaves", lower=1L), 
#      makeNumericLearnerParam(id="penalty", lower=0),
#      makeIntegerLearnerParam(id="seed"),
#      makeDiscreteLearnerParam(id="select", default=1L, values=c(1L,6L), pass.default=TRUE),
#      makeIntegerLearnerParam(id="treesize", default=8L, lower=1), 
#      makeDiscreteLearnerParam(id="opers", default=1L, values=1:3), 
#      makeIntegerLearnerParam(id="minmass", default=0L, lower=0L) 
#    ), 
#    missings = FALSE,
#    numerics = TRUE,
#    factors = FALSE,
#    se = FALSE,
#    weights = TRUE
#  )
#}
#
#trainLearner.regr.logicreg = function(.learner, .task, .subset,  ...) {
#  xs = learnerArgsToControl(logreg.tree.control, c("treesize", "opers", "minmass"), list(...))
#  d = getTaskData(.task, .subset, target.extra=TRUE)
#  logreg(bin=d$data, resp=d$target, type=2, tree.control=xs$control, 
#    select=select, ntrees=ntrees, nleaves=nleaves, penalty=penalty, seed=seed)
#}
#
#predictLearner.regr.logicreg = function(.learner, .model, .newdata, ...) {
#  predict(.model$learner.model, newbin=.newdata, ...)
#}