load_all()
library(h2o)

#TODO: h2oinit??
# welche algo can weights? glm mindestens
# wie bekommt man probs vom glm? und classen? 
# -> Bug in h2o.glm
# regr.glm: was sind die families gamma und tweedy? nehmen wir die rein?
# kann man predtype="se" für regr modelle bekommen? wenn ja für welche?
# glm hat varimp. wo noch? h2o.varimp gucken
h2o.init()
configureMlr(show.learner.output = F)

classif = c("classif.h2ogbm", "classif.h2oglm", "classif.h2orandomForest", "classif.h2odeeplearning")
for(i in classif){
  lrn = makeLearner(i, predict.type = "prob")
  holdout(lrn, sonar.task, measures = list(acc))
}

# lrn = makeLearner("classif.h2ogbm", predict.type = "prob")
# lrn = makeLearner("regr.h2ogbm")
lrn = makeLearner("classif.logreg", predict.type = "prob")
# lrn = makeLearner("regr.h2oglm")
lrn = makeLearner("classif.h2orandomForest", predict.type = "prob")
# lrn = makeLearner("regr.h2orandomForest")
m = train(lrn, sonar.task)
p = predict(m, sonar.task)
r = holdout(lrn, bh.task, measures = list(mse))
#FIXME: warum wird bei den wights nicht gecheck ob die korrekt lang sind?
r = holdout(lrn, sonar.task, weights = 1:20)
