library("devtools")
load_all()
source("todo-files/MulticlassWrapper.R")
source("tests/testthat/helper_objects.R")
lrn = makeLearner(cl="classif.lda", id="bla")
.task = multiclass.task

.learner = makeMulticlassWrapper(learner=lrn)
#.learner = makeBaggingWrapper(learner=lrn, bw.iters=10)
mod = .model = train(learner=.learner, task=multiclass.task)
mod

# Warum wird hier predictLearner.BaseWrapper aufgerufen?
# Bei BaggingModel wird direkt predictLearner.BaggingWrapper aufgerufen, so wie es sein sollte
predict(mod, newdata=multiclass.test)
pred = predictLearner(.learner=.learner, .model=.model, .newdata=multiclass.test)


