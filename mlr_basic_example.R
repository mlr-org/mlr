library(mlr)

# Beispiel
data(iris)
iris <- data.frame(iris,as.factor(sample(1:3,150,replace=T)))
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
r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE, measures = list(mmce, ber))
r2 = resample(learner = lrn, task = task2, resampling = rdesc, show.info = FALSE)
a <- traceback()

r2$pred$data$V1
## Get the mean misclassification error:
r2$aggr

