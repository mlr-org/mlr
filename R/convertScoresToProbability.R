#' @title Convert anomaly scores to probability estimates.
#'
#' @description Convert anomaly scores to probability estimates with the sigmoid function (calibration function) 1 / (1 + exp(-(A + B * score))). The higher the probability estimate the more likely the observation belongs to the anomaly class.
#'
#' @param anomaly.score a numeric vector of anomaly scores.
#' @param param a vector of values for the parameter A and B of the sigmoid function.
#' Default ist A = 0 and B = 1
#' @return [\code{vector}] with probabilities as entries.
#' @export
#' @references Gao, Jing, and Pang-Ning Tan. "Converting output scores from outlier detection algorithms into probability estimates." Data Mining, 2006. ICDM'06. Sixth International Conference on. IEEE, 2006.
#' @examples
#'
#' Data = oneclass2d.task$env$data # getTaskData(oneclass2d.task)
#' svm.model = e1071::svm(Data[,1:2], y = NULL, type = 'one-classification',
#' kernel = "radial", nu = 0.05)
#  svm.pred = predict(svm.model, Data[,1:2])
#' dv = svm.model$decision.values
#' prob = convertingScoresToProbability(dv, param = c(0, 1))$probability
#' o = order(prob)
#' col = factor(Data$Target, levels = c("Normal", "Anomaly"), labels =  c("black", "red"))
#' plot(1:length(prob), prob[o], col = col[o])

convertingScoresToProbability = function(anomaly.score, param = c(0, 1)){

  f = anomaly.score
  list = list()

  if (length(param) != 2) {
    stop("Too little/many starting parameters for optimization.")
  }

  # probability for outlier given the scores
  prob.outlier = function(p, score) {
    1 / (1 + exp(-p[2] * score - p[1]))
  }

  list$p = param
  list$probability = prob.outlier(param, f)
  list
}
