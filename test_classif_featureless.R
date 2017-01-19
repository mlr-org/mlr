load_all()

configureMlr(show.learner.output = TRUE, on.learner.error = "stop")
options(warn = 2)

# lrn = makeLearner("classif.dummy", method = "sample-prior", predict.type = "prob")
# m = train(lrn, iris.task)
# p = predict(m, iris.task)
# print(summary(as.data.frame(p)$response))
# print(head(as.data.frame(p)))
#

lrn = makeLearner("regr.dummy", method = "median", predict.type = "se")
m = train(lrn, bh.task)
p = predict(m, bh.task)
print(summary(as.data.frame(p)$response))
print(head(as.data.frame(p)))
#
