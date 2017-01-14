setwd("~/work/mlr-org/mlr")
library(BBmisc)
library(stringi)
load_all()

configureMlr(show.info = TRUE)
# getMlrOptions()

lrn = makeLearner("classif.ksvm", predict.type = "prob")
meas = list(mmce, acc, logloss)
rin = resample(lrn, iris.task, cv5, meas)



mod = train(lrn, iris.task)
preds = predict(mod, iris.task)
perfs = performance(preds, measures = meas)

y = formatC(perfs, digits = 3L)

stri_paste(y,  collapse = " ")




measure.ids = collapse(extractSubList(meas, "id"), sep = " ")
measure.ids


sprintf("%s    ", cv10$id)

sprintf("%s %s", cv10$id, measure.ids)

names(rin)

class(cv10)