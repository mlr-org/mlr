load_all()
library(h2o)

#TODO: h2oinit??
# welche algo can weights? glm mindestens
# wie bekommt man probs vom glm? und classen?
# regr.glm: was sind die families gamma und tweedy? nehmen wir die rein?
# kann man predtype="se" für regr modelle bekommen? wenn ja für welche?
# glm hat varimp. wo noch? h2o.varimp gucken
h2o.init()
configureMlr(show.learner.output = F)

# lrn = makeLearner("classif.h2ogbm", predict.type = "prob")
# lrn = makeLearner("regr.h2ogbm")
# lrn = makeLearner("classif.h2oglm")
lrn = makeLearner("regr.h2oglm")
# lrn = makeLearner("classif.h2orandomForest", predict.type = "prob")
# lrn = makeLearner("regr.h2orandomForest")
# m = train(lrn, iris.task)
# p = predict(m, iris.task)
# r = holdout(lrn, sonar.task)
#FIXME: warum wird bei den wights nicht gecheck ob die korrekt lang sind?
r = holdout(lrn, bh.task, weights = 1:20)
