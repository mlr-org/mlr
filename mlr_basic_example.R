library(devtools)
load_all("/media/philipp/DATA/Studium/12.Semester/Masterarbeit/mlr_multilabel/mlr")

library(mlr)

# Beispiel
data(iris)
iris <- data.frame(iris,as.factor(sample(c("TRUE","FALSE"),150,replace=T)))
colnames(iris)[6] <- "neu"
## Define the task:
task = makeClassifTask(id = "tutorial", data = iris[-c(1:10),], target = "Species")
task2 = makeMultilabelTask(id="multi",data = iris[-c(1:10),], target= c("Species","neu"))

## Define the learner:
lrn = makeLearner("classif.randomForest")
## Train 
tr = train(lrn,task)
tr2 = train(lrn,task2)

## Predict
pr = predict(tr,newdata=iris[1:10,])
pr2 = predict(tr2,newdata=iris[1:10,])

## Define the resampling strategy:
rdesc = makeResampleDesc(method = "CV", stratify = FALSE)
## Do the resampling:
r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE)
r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE, measures = list(timetrain,mmce))

r2 = resample(learner = lrn, task = task2, resampling = rdesc, show.info = FALSE, measures = list(hamloss,timetrain))
r2 = resample(learner = lrn, task = task2, resampling = rdesc, show.info = FALSE, measures = list(mmce))
r2 = resample(learner = lrn, task = task2, resampling = rdesc, show.info = FALSE, measures = list(hamloss,mmce,acc,timetrain))
a <- traceback()

r2$pred$data$V1
## Get the mean misclassification error:
r2$aggr

# With probabilities
lrn = makeLearner("classif.randomForest",predict.type="prob")
task = makeClassifTask(id = "tutorial", data = iris[-c(1:10),], target = "neu")
tr = train(lrn,task)
pr = predict(tr,newdata=iris)
plotROCRCurves(pr,diagonal=T)

task = makeMultilabelTask(id = "tutorial", data = iris[-c(1:10),], target = c("neu","Species"))
tr = train(lrn,task)
pr = predict(tr,newdata=iris[1:10,])



lrn1 = makeLearner("classif.lda", predict.type = "prob")
mod1 = train(lrn1, sonar.task)
pred1 = predict(mod1, task = sonar.task)
plotROCRCurves(pred1)
