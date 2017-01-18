load_all()

configureMlr(show.info = TRUE)

# load gunpoint data
gp = load2("demo4TS/gunpoint.RData")

task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")

lrn = makeLearner("tsclassif.shapelet")


model = train(lrn, task, subset = 1:50)
pred = predict(model, task, subset = 51:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)


# create the sample GP splits as on UCR page, they used the first 50 for train, last 150 for test
# then resample (this is holdout eval)
rin = makeFixedHoldoutInstance(size = 200, train.inds = 1:50, test.inds = 51:200)
r = resample(lrn, task, rin)
print(r)




ctrl = makeTuneControlRandom(maxit = 2)
ps = makeParamSet(
  makeNumericParam("k", lower = 0.1, upper = 0.2),
  makeNumericParam("l", lower = 0.01, upper = 0.2)
)


tr = tuneParams(lrn, task, rin, par.set = ps, control = ctrl)
opd = as.data.frame(tr$opt.path)
op2 = trafoOptPath(tr$opt.path)
opd2 = as.data.frame(op2)
#parallelStop()
print(tr)
