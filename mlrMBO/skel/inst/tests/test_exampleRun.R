context("exampleRun")

#FIXME renable
# FIXME: add a text with discrete and factor vecs

# test_that("exampleRun", {
# 
#   doRun = function(f, par.set, predict.type, crit) {
#     learner = makeLearner("regr.randomForest", ntree=3, predict.type=predict.type)
#     ctrl = makeMBOControl(init.design.points=20, iters=2, infill.crit=crit,
#       infill.opt="random", infill.opt.random.points=10)
#     run = exampleRun(makeMBOFunction(f), par.set, learner, ctrl)
#     plot(run)
#   }
# 
#   f = function(x) sum(x*x)
#   ps = makeParamSet(
#     makeNumericParam("x", lower=-2, upper=2)
#   )
#   # with se
#   doRun(f, ps, "response", "mean")
#   # with se
#   doRun(f, ps, "se", "ei")
#     # without se
# })
