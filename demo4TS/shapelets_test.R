load_all()

configureMlr(show.info = TRUE)

# load gunpoint data
gp = load2("demo4TS/gunpoint.RData")


task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")

# # start with a simple CART tree (its simple and fast)
lrn = makeLearner("tsclassif.shapeletClass")

# # train, predict, measure perf, all manual
# # (and use complete data, so insample eval
model = train(learner = lrn, task = task, subset = 1:50)
