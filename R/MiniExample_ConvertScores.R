library(e1071)
data = readRDS("R/anomaly.data.5percent.rds")

# data = readRDS("banana.rds")
# speach = readRDS("speach.rds")
# shuttle = readRDS("shuttle.rds")
# satellite = readRDS("satellite.rds")
# pen.local = readRDS("pen.local.rds")
# pen.global = readRDS("pen.global.rds")
# letter = readRDS("letter.rds")
# kdd99  = readRDS("kdd99.rds")
# breast.cancer = readRDS("breast.cancer.rds")
# annthyroid = readRDS("annthyroid.rds")
# aloi = readRDS("aloi.rds")
# artificial.unsup = readRDS("artificial.unsup.rds")
#
# cover = readRDS("cover.rds")
# http = readRDS("http.rds")
# mammography = readRDS("mammography.rds")
# mulcross = readRDS("mulcross.rds")
# smtp = readRDS("smtp.rds")


col = factor(data$Target, levels = c("Normal", "Anomaly"), labels =  c("black", "red"))
data$Target = NULL

# nu 0.5 per default
svm.model0.5 <- svm(data, y = NULL, type='one-classification', nu = 0.5, kernel="radial", scale=TRUE)
svm.pred0.5 <- predict(svm.model0.5, data, decision.values = TRUE)
scores0.5 = attr(svm.pred0.5, "decision.values")
convert0.5 = convertingScoresToProbability(scores0.5, parainit = c(0,1), optim.method = "glm")
prob0.5 = convert0.5$probability
convert0.5$p
o0.5 = order(scores0.5)

# nu 0.09
svm.model0.09 <- svm(data, y = NULL, type='one-classification', nu = 0.09, kernel="radial", scale=TRUE)
svm.pred0.09 <- predict(svm.model0.09, data, decision.values = TRUE)
scores0.09 = attr(svm.pred0.09, "decision.values")
convert0.09 = convertingScoresToProbability(scores0.09, parainit = c(0,1), optim.method = "glm")
prob0.09 = convert0.09$probability
convert0.09$p
o0.09 = order(scores0.09)

# 0.05 (erwartet 5% anomaly)
svm.model0.05 <- svm(data, y = NULL, type='one-classification', nu = 0.05, kernel="radial", scale=TRUE)
svm.pred0.05 <- predict(svm.model0.05, data, decision.values = TRUE)
scores0.05 = attr(svm.pred0.05, "decision.values")
convert0.05 = convertingScoresToProbability(scores0.05, parainit = c(0,1), optim.method = "glm")
prob0.05 = convert0.05$probability
convert0.05$p
o0.05 = order(scores0.05)


par(mfrow = c(3,2))
plot(1:length(scores0.5), scores0.5[o0.5], col = col[o0.5], main = "scores - nu=0.5")
plot(1:length(prob0.5), prob0.5[o0.5], col = col[o0.5], main = "prob - nu=0.5")

plot(1:length(scores0.09), scores0.09[o0.09], col = col[o0.09], main = "scores - nu=0.09")
plot(1:length(prob0.09), prob0.09[o0.09], col = col[o0.09], main = "prob- nu=0.09")

plot(1:length(scores0.05), scores0.05[o0.05], col = col[o0.05], main = "scores - nu=0.05")
plot(1:length(prob0.05), prob0.05[o0.05], col = col[o0.05], main = "prob - nu=0.05")

