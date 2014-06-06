load_all(".")

 bls = list(
   makeLearner("classif.ksvm"),
   makeLearner("classif.randomForest")
 )
 lrn = makeModelMultiplexer(bls)
 rdesc = makeResampleDesc("CV", iters = 2L)
 ps = makeParamSet(
   makeDiscreteParam("selected.learner", values = extractSubList(bls, "id")),
   makeNumericParam("classif.ksvm.sigma", lower=-10, upper = 10, trafo = function(x) 2^x,
     requires = quote(selected.learner == "classif.ksvm")),
   makeIntegerParam("classif.randomForest.ntree", lower = 1L, upper = 500L, 
     requires = quote(selected.learner == "classif.randomForsest"))
 )
 ctrl = makeTuneControlIrace(maxExperiments = 200)
 res = tuneParams(lrn, iris.task, rdesc, par.set = ps, control = ctrl)
 print(res)
 print(head(as.data.frame(res$opt.path)))

# data = bh.task
# data[1, 1] = NA
# data[2, 2] = NA
# task = makeClassifTask(data = data, target = "Species")
# m = train("classif.lda", task)

# f = getFilterValues(bh.task, method = fm)
# print(plotFilterValues(bh.task, f, sort = "dec", nshow = 20, feat.type.cols = NULL))


# mytask = bh.task 
# mytask = dropFeatures(mytask, "chas")
# for (fm in getFilterMethods()) {
  # print(fm)
  # f = getFilterValues(mytask, method = fm)
  # print(f)
# }
#
#


# f = filterFeatures(iris.task, val = 0.5)

# print(f)
