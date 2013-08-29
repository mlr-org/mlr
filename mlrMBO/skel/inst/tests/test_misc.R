#FIXME reread and fix!

#context("simple optims")

#test_that("simple optims", {
  # maximum is x1 = 50, x2 = any, x3=a, y=70
#   fit = function(x) {
#     #print(x)
#     x$x1 - is.null(x$x2) * 100 + 20 * (x$x3 == "a")
#   }
#   
#   easy = makeParamSet(
#     makeNumericParam("x1", lower = 0, upper = 100),
#     makeNumericParam("x2", lower = 50, upper = 100, requires = quote(x1 <= 50)),
#     makeDiscreteParam("x3", values = c("a", "blasdfkjaslkdjaslkdjflkjsdafj")))
#   
#   surrogate = makeLearner("regr.randomForest")
#   ctrl = makeMBOControl(minimize=FALSE, infill.crit="mean", iters=30, infill.opt.random.points=100)  
#   opt = mbo(fit, easy, learner = surrogate, control= ctrl)
#   expect_true(opt$x$x1 > 45 && opt$x$x1 <= 50 && is.na(opt$x$x2) && opt$x$x3 == "a" && opt$y > 68 && opt$y <= 70)
#})

#  test_that("complex paramset" , {
#   if(isExpensiveExampleOk()) {
#     ps = makeParamSet(
#       makeLogicalParam("use.clinical"),
#       makeDiscreteParam("filter", values = c("none", "var", "uni", "kratz", "top21", "fmrmr", "pamr")),
#       makeDiscreteParam("model", values = c("coxboost", "glmnet", "rfsrc", "penalized", "rpart")),
# 
#       makeLogicalParam("filter.use.clinical",
#         requires = quote(filter == "uni" && isTRUE(use.clinical))),
#       makeNumericParam("filter.perc", lower=0, upper=1/3,
#         requires = quote(filter %in% c("var", "uni", "fmrmr"))),
#       makeDiscreteParam("filter.fmrmr.combine", values = c("difference", "quotient"),
#         requires = quote(filter == "fmrmr")),
#       makeDiscreteParam("filter.fmrmr.relevance", values = c("cindex"),
#         requires = quote(filter == "fmrmr")),
#       makeDiscreteParam("filter.fmrmr.redundance", values = c("mi", "pearson"),
#         requires = quote(filter == "fmrmr")),
#       makeIntegerParam("filter.pamr.ngroup.survival", lower = 2L, upper = 3L,
#         requires = quote(filter == "pamr")),
# 
#       makeNumericParam("coxboost.stepsize.factor", lower=0.1, upper=2,
#         requires = quote(model == "coxboost")),
#       makeNumericParam("coxboost.penalty", lower = 0.1, upper = 300,
#         requires = quote(model == "coxboost")),
#       makeDiscreteParam("penalized.penalty", values = c("L1", "L2"),
#                         requires = quote(model == "penalized")),
#       makeNumericParam("glmnet.alpha", lower = 0, upper = 1,
#         requires = quote(model == "glmnet")),
#       makeDiscreteParam("rfsrc.splitrule", values = c("logrank", "logrankscore"),
#         requires = quote(model == "rfsrc")),
#       makeIntegerParam("rfsrc.nodesize", lower = 1L, upper = 10L,
#         requires = quote(model == "rfsrc")),
#       makeIntegerParam("rfsrc.ntree", lower = 50L, upper = 2000L,
#         requires = quote(model == "rfsrc")),
#       makeIntegerParam("rpart.minsplit", lower=1L, upper=30L,
#         requires = quote(model == "rpart")),
#       makeIntegerParam("rpart.minbucket", lower=1L, upper=15L,
#         requires = quote(model == "rpart")),
#       makeNumericParam("rpart.cp", lower=0.001, upper=0.1,
#         requires = quote(model == "rpart"))
#     )
# 
#     fit = function(x) {
#       sum(sapply(x, nchar))
#     }
# 
#     surrogate = makeLearner("regr.randomForest")
#     opt = mbo(fit, ps, learner = surrogate, control = makeMBOControl(infill.opt.random.points=10))
#   }
# })
