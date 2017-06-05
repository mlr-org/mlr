#' @title Convert anomaly scores to probability estimates.
#'
#' @description Convert anomaly scores to probability estimates with the sigmoid function (calibration function). The higher the probability estimate the more likely the observation belongs to the normal class.
#'
#' @param anomaly.score a numeric vector of anomaly scores.
#' @param parainit a vector of starting values for the optimizer
#' @param max.iter Maximum number of iterations. Defaults to 100
#' @return [\code{vector}] with probabilities as entries.
#' @export
#' @references Gao, Jing, and Pang-Ning Tan. "Converting output scores from outlier detection algorithms into probability estimates." Data Mining, 2006. ICDM'06. Sixth International Conference on. IEEE, 2006.
#' @examples
#'
#' # Data = data[, 1:4] # find better dataset later
#' svm.model <- svm(Data, y = NULL, type = 'one-classification', kernel = "radial", nu = 0.05)
#  svm.pred <- predict(svm.model, Data)
#' dv = svm.model$decision.values
#' prop = convertingScoresToProbability(dv, parainit = c(0, 1), method = "sigmoid")
#' plot(1:length(prop$probability), prop$probability, ylim = c(0, 1))
#'
convertingScoresToProbability = function(anomaly.score, parainit = NULL, max.iter = 100){
  f = anomaly.score
  p = parainit
  loop = TRUE
  sum.help = rep(1, length(f))
  list = list()

  if (is.null(p)) {
    p = c(A = 1, B = 0)
    messagef("Starting parameter values are missing. For calibration using sigmoid function the default is: \n A = 1, B = 0")
  }
  if (length(p) != 2) {
    stop("Too little/many starting parameters. For calibration using sigmoid function starting values for parameter A and B are needed.")
  }

  while (loop) {
    t = ifelse(p[1] * f + p[2] > 0, 1, 0)
    # LL and its derivatives
    LL = function(p) { t((1-t)) %*% (p[1] * f + p[2]) + sum.help %*% log(1 + exp(-p[1] * f - p[2])) }
    gA = function(p) { t(f) %*% ( (1-t) - (1 / exp(p[1] * f + p[2]))) }
    gB = function(p) { sum.help %*% ((1-t) - (1 / exp(p[1]*f + p[2]))) }
    g = function(p) {
      c(gA(p), gB(p))
    }

    optim = trust.optim(p, fn = LL, gr = g,  method = "BFGS", control = list(report.level = 0, maxit = max.iter))
    pnew = optim$solution
    diff = sum(abs(pnew - p))

    if ( diff > 1e-4) {
      loop = TRUE
      p = pnew
    } else {
      loop = FALSE
      probability = 1 / (1 + exp(-pnew[1] * f - pnew[2]))
    }
  }
  return(probability)
}

