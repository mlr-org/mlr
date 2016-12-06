load_all()
library('BBmisc')
configureMlr(show.info = TRUE, show.learner.output = TRUE)

gp = load2("demo4TS/gunpoint.RData")
task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")

lrn = makeLearner("tsclassif.shapelet")

# # train, predict, measure perf, all manual
# # (and use complete data, so insample eval....)
model = train(lrn, task, subset = 1:50)
pred = predict(model, task, subset = 51:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)

# create the sample GP splits as on UCR page, they used the first 50 for train, last 150 for test
# then resample (this is holdout eval)
# rin = makeFixedHoldoutInstance(size = 200, train.inds = 1:50, test.inds = 51:200)
# r = resample(lrn, task, rin)
# print(r)


