load_all()

d = iris[seq(1, 150, 3), ]
d[1,1] = NA_real_
task = makeClassifTask(data = d, target = "Species")
lrn = makeImputeWrapper("classif.rpart", classes = list(numeric = imputeMedian()))
m = train(lrn, task)
# p = predict(m, task)
# expect_true(!any(is.na(p$data$response)))
# mm = getLearnerModel(m, more.unwrap = TRUE)
# expect_output(print(mm), "root")
# expect_is(mm, "rpart")
# mm = getLearnerModel(m, more.unwrap = FALSE)
# expect_output(print(mm), "Model")

