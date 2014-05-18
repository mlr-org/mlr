\name{learners}
\alias{learners}
\title{List of supported learning algorithms.}
\description{
\itemize{
  \item{\bold{classif.ada}}{\cr Boosting from ada package: \code{\link[ada]{ada}}}
  \item{\bold{classif.boosting}}{\cr Boosting from adabag package: \code{\link[adabag]{boosting}}\cr
    Note that \code{xval} has been set to 0 by default for speed.}
  \item{\bold{classif.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
  \item{\bold{classif.ctree}}{\cr Conditional Inference Trees from party package: \code{\link[party]{ctree}}}
  \item{\bold{classif.fnn}}{\cr Fast k-Nearest Neighbor from FNN package: \code{\link[FNN]{knn}}}
  \item{\bold{classif.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
  \item{\bold{classif.geoDA}}{\cr Geometric Predictive Discriminant Analysis from DiscriMiner package: \code{\link[DiscriMiner]{geoDA}}}
  \item{\bold{classif.glmboost}}{\cr Boosting for GLMs from mboost package: \code{\link[mboost]{glmboost}}\cr
    Note that \code{family} has been set to \code{Binomial()} by default.}
  \item{\bold{classif.J48}}{\cr J48 Decision Trees from RWeka package: \code{\link[RWeka]{J48}}}
    Note that NAs are directly passed to WEKA with \code{na.action = na.pass}.
  \item{\bold{classif.JRip}}{\cr Propositional Rule Learner from RWeka package: \code{\link[RWeka]{JRip}}}
    Note that NAs are directly passed to WEKA with \code{na.action = na.pass}.
  \item{\bold{classif.kknn}}{\cr k-Nearest Neighbor from kknn package: \code{\link[kknn]{kknn}}}
  \item{\bold{classif.knn}}{\cr k-Nearest Neighbor from class package: \code{\link[class]{knn}}}
  \item{\bold{classif.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}}\cr
    Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.\cr
    Note that \code{fit} has been set to \code{FALSE} by default for speed.}
  \item{\bold{classif.lda}}{\cr Linear Discriminant Analysis from MASS package: \code{\link[MASS]{lda}}}
  \item{\bold{classif.LiblineaRBinary}}{\cr Regularized Binary Linear Predictive Models Estimation from LiblineaR package: \code{\link[LiblineaR]{LiblineaR}}
    Note that this model subsumes the types 1,2,3,5}
  \item{\bold{classif.LiblineaRLogReg}}{\cr Regularized Logistic Regression from LiblineaR package: \code{\link[LiblineaR]{LiblineaR}}
    Note that this model subsumes type 0,6,7.}
  \item{\bold{classif.LiblineaRMultiClass}}{\cr Multi-class Support Vector Classification by Crammer and Singer from LiblineaR package: \code{\link[LiblineaR]{LiblineaR}}
    Note that this model is type 4.}
  \item{\bold{classif.linDA}}{\cr Linear Discriminant Analysis from DiscriMiner package: \code{\link[DiscriMiner]{linDA}}}
  \item{\bold{classif.logreg}}{\cr Logistic Regression from stats package: \code{\link[stats]{glm}}}
  \item{\bold{classif.lssvm}}{\cr Least Squares Support Vector Machine from kernlab package: \code{\link[kernlab]{lssvm}}\cr
    Note that \code{fitted} has been set to \code{FALSE} by default for speed.}
  \item{\bold{classif.lvq1}}{\cr Learning Vector Quantization from class package: \code{\link[class]{lvq1}}}
  \item{\bold{classif.mda}}{\cr Mixture Discriminant Analysis from mda package: \code{\link[mda]{mda}}\cr
    Note that \code{keep.fitted} has been set to \code{FALSE} by default for speed.}
  \item{\bold{classif.multinom}}{\cr Multinomial Regression from nnet package: \code{\link[nnet]{multinom}}}
  \item{\bold{classif.naiveBayes}}{\cr Naive Bayes from e1071 package: \code{\link[e1071]{naiveBayes}}}
  \item{\bold{classif.nnet}}{\cr Neural Network from nnet package: \code{\link[nnet]{nnet}}\cr
    Note that \code{size} has been set to 3 by default.}
  \item{\bold{classif.OneR}}{\cr 1-R classifier from RWeka package: \code{\link[RWeka]{OneR}}\cr
    Note that NAs are directly passed to WEKA with \code{na.action = na.passi}}.
  \item{\bold{classif.PART}}{\cr PART decision lists from RWeka package: \code{\link[RWeka]{PART}}\cr
    Note that NAs are directly passed to WEKA with \code{na.action = na.pass}}.
  \item{\bold{classif.plr}}{\cr Logistic regression with a L2 penalty from stepPlr package: \code{\link[stepPlr]{plr}}\cr
    Note that AIC and BIC penalty types can be selected via the new parameter \code{cp.type}}.
  \item{\bold{classif.plsDA}}{\cr Partial Least Squares (PLS) Discriminant Analysis from DiscriMiner package: \code{\link[DiscriMiner]{plsDA}}}
  \item{\bold{classif.qda}}{\cr Quadratic Discriminant Analysis from MASS package: \code{\link[MASS]{qda}}}
  \item{\bold{classif.quaDA}}{\cr Quadratic Discriminant Analysis from DiscriMiner package: \code{\link[DiscriMiner]{quaDA}}}
  \item{\bold{classif.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}.\cr
    The argument \code{fix.factors} restores the factor levels seen in the training data before prediction to circumvent
    randomForest's internal sanity checks. Default is \code{FALSE}.}
  \item{\bold{classif.rda}}{\cr Regularized Discriminant Analysis from klaR package: \code{\link[klaR]{rda}}\cr
    Note that \code{estimate.error} has been set to \code{FALSE} by default for speed.}
  \item{\bold{classif.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}\cr
    Note that \code{xval} has been set to 0 by default for speed.}
  \item{\bold{classif.svm}}{\cr Support Vector Machines (libsvm) from e1071 package: \code{\link[e1071]{svm}}}
}

\itemize{
  \item{\bold{regr.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
  \item{\bold{regr.crs}}{\cr Regression Splines from crs package: \code{\link[crs]{crs}}}
  \item{\bold{regr.earth}}{\cr Multivariate Adaptive Regression Splines from earth package: \code{\link[earth]{earth}}}
  \item{\bold{regr.fnn}}{\cr Fast k-Nearest Neighbor from FNN package: \code{\link[FNN]{knn}}}
  \item{\bold{regr.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}\cr
    Note that \code{distribution} has been set to \dQuote{gaussian} by default.}
  \item{\bold{regr.kknn}}{\cr K-Nearest-Neighbor regression from kknn package: \code{\link[kknn]{kknn}}}
   \item{\bold{regr.km}}{\cr Kriging from DiceKriging package: \code{\link[DiceKriging]{km}}}
  \item{\bold{regr.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}}\cr
    Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.\cr
    Note that \code{fit} has been set to \code{FALSE} by default for speed.}
  \item{\bold{regr.penalized.lasso}}{\cr Lasso regression from penalized package: \code{\link[penalized]{penalized}}}
  \item{\bold{regr.lm}}{\cr Simple linear regression from stats package: \code{\link[stats]{lm}}}
  \item{\bold{regr.mars}}{\cr Multivariate Adaptive Regression Splines from mda package: \code{\link[mda]{mars}}}
  \item{\bold{regr.mob}}{\cr Model-based recursive partitioning  yielding a tree with fitted models associated
    with each terminal node from party package: \code{\link[party]{mob}}}
  \item{\bold{regr.nnet}}{\cr Neural Network from nnet package: \code{\link[nnet]{nnet}}\cr
    Note that \code{size} has been set to 3 by default.}
  \item{\bold{regr.pcr}}{\cr Principal component regression from pls package: \code{\link[pls]{pcr}}\cr
    Note that \code{model} has been set to \code{FALSE} by default for speed.}
  \item{\bold{regr.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}.
    The argument \code{fix.factors} restores the factor levels seen in the training data before prediction to circumvent
    randomForest's internal sanity checks. Default is \code{FALSE}.}
  \item{\bold{regr.penalized.ridge}}{\cr Ridge regression from penalized package: \code{\link[penalized]{penalized}}}
  \item{\bold{regr.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}\cr
    Note that \code{xval} has been set to 0 by default for speed.}
  \item{\bold{regr.rsm}}{\cr Response surface regression from rsm package: \code{\link[rsm]{rsm}}\cr
    Note that you select the order of the regression by using modelfun="FO" (first order), "TWI" (two-way interactions, this is with 1st oder terms!) and "SO" (full second order)}.
  \item{\bold{regr.rvm}}{\cr Relevance Vector Machine from package kernlab: \code{\link[kernlab]{rvm}}\cr
    Note that kernel parameters have to be passed directly and not by using the kpar list in rvm.\cr
    Note that \code{fit} has been set to \code{FALSE} by default for speed.}
}
\itemize{
  \item{\bold{surv.CoxBoosth}}{\cr Cox proportional hazards model with componentwise likelhood based boosting from CoxBoost package: \code{\link[CoxBoost]{CoxBoost}}}
  \item{\bold{surv.coxph}}{\cr Cox proportional hazard model from survival package: \code{\link[survival]{coxph}}}
  \item{\bold{surv.glmnet}}{\cr GLM with regularization from glmnet package: \code{\link[glmnet]{glmnet}}}
}
}

