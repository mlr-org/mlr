library(devtools)
library(testthat)

load_all(".")
source("todo-files/getROCCoord.R")

# tt = as.factor(c("a", "b", "b"))
# pp =           c(0.1, 0.2, 0.7)
# mm = data.frame(b = yy, a = 1-yy)
# task = makeClassifTask(data = data.frame(x = rnorm(length(tt)), y = tt), target = "y", positive = "b")
# p = makePrediction(task$task.desc, NULL, id = "foo", predict.type = "prob", truth = tt, y = mm, time = 0)
# rc = getROCCoords(p)

# expect_equal(rc, as.data.frame(matrix(c(
#   1.0, 0.0, 0,
#   0.7, 0.5, 0,
#   0.2, 1.0, 0,
#   0.1, 1.0, 1,
#   0.0, 1.0, 1
# ), byrow = TRUE, ncol = 3L)), check.names = FALSE)

# plotROCCurve(p)

 lrn = makeLearner("classif.lda", predict.type = "prob")
 mod = train(lrn, sonar.task)
 ps = predict(mod, sonar.task)
 coords = getROCCoords(ps)
 # library(ggplot2)
 # print(
 # ggplot(coords, aes(x = fpr, y = tpr)) +
 # geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = 0.5) +
 # geom_path() +
 # theme_bw() +
 # coord_fixed(ratio = 1)
# )


 # lrn1 = makeLearner("classif.lda", predict.type = "prob")
 # lrn2 = makeLearner("classif.ksvm", predict.type = "prob")
 # rdesc = makeResampleDesc("CV", iters = 10)
 # res = benchmark(list(lrn1, lrn2), list(pid.task, sonar.task), rdesc)
 # coords = getROCCoords(res)
# print(
 # ggplot(coords, aes(x = fpr, y = tpr, group = learner, color = learner)) +
 # geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = 0.5) +
 # geom_path() +
 # theme_bw() +
 # coord_fixed(ratio = 1) +
 # facet_grid(. ~ task)
# )

