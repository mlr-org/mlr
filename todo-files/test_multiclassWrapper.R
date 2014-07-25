library("devtools")
load_all()
source("tests/testthat/helper_objects.R")
.learner = makeLearner(cl="classif.lda", id="bla")
.task = multiclass.task

lrn = makeMulticlassWrapper(learner=.learner)
#lrn = makeBaggingWrapper(learner=.learner, bw.iters=10)
mod = .model = train(learner=lrn, task=multiclass.task)
class(mod)
predict(mod, newdata=multiclass.test)

