#' Iris classification task.
#'
#' Contains the task (\code{iris.task}).
#'
#' @name iris.task
#' @references See \code{\link[datasets]{iris}}.
#' @keywords data
#' @docType data
NULL

#' Artificial oneclass toy task.
#'
#' Contains the task (\code{oneclass2d.task}).
#'
#' The data for this class has two feature variables (V1, V2)
#' and is simulated with 1000 normal observations and 50 anomalous observations.
#' The features of the normal class were drawn from a multivariate normal distribution with zero mean
#' and a diagonal covariance matrix with variances (2,5).
#' The anomaly class was sampled with feature value uniformly distributed between 20 and 100.
#'
#' @name oneclass2d.task
#' @keywords data
#' @docType data
NULL

#' Sonar classification task.
#'
#' Contains the task (\code{sonar.task}).
#'
#' @name sonar.task
#' @references See \code{\link[mlbench]{Sonar}}.
#' @keywords data
#' @docType data
NULL

#' Wisconsin Breast Cancer classification task.
#'
#' Contains the task (\code{bc.task}).
#'
#' @name bc.task
#' @references See \code{\link[mlbench]{BreastCancer}}.
#'   The column \code{"Id"} and all incomplete cases have been removed from the task.
#' @keywords data
#' @docType data
NULL

#' PimaIndiansDiabetes classification task.
#'
#' Contains the task (\code{pid.task}).
#'
#' @name pid.task
#' @references See \code{\link[mlbench]{PimaIndiansDiabetes}}.
#'   Note that this is the uncorrected version from mlbench.
#' @keywords data
#' @docType data
NULL

#' Boston Housing regression task.
#'
#' Contains the task (\code{bh.task}).
#'
#' @name bh.task
#' @aliases bh.task
#' @references See \code{\link[mlbench]{BostonHousing}}.
#' @keywords data
#' @docType data
NULL

#' Wisonsin Prognostic Breast Cancer (WPBC) survival task.
#'
#' Contains the task (\code{wpbc.task}).
#'
#' @name wpbc.task
#' @aliases wpbc.task
#' @references See \code{\link[TH.data]{wpbc}}.
#'  Incomplete cases have been removed from the task.
#' @keywords data
#' @docType data
NULL

#' NCCTG Lung Cancer survival task.
#'
#' Contains the task (\code{lung.task}).
#'
#' @name lung.task
#' @aliases lung.task
#' @references See \code{\link[survival]{lung}}.
#'  Incomplete cases have been removed from the task.
#' @keywords data
#' @docType data
NULL

#' Motor Trend Car Road Tests clustering task.
#'
#' Contains the task (\code{mtcars.task}).
#'
#' @name mtcars.task
#' @aliases mtcars.task
#' @references See \code{\link[datasets]{mtcars}}.
#' @keywords data
#' @docType data
NULL

#' European Union Agricultural Workforces clustering task.
#'
#' Contains the task (\code{agri.task}).
#'
#' @name agri.task
#' @aliases agri.task
#' @references See \code{\link[cluster]{agriculture}}.
#' @keywords data
#' @docType data
NULL

#' Iris cost-sensitive classification task.
#'
#' Contains the task (\code{costiris.task}).
#'
#' @name costiris.task
#' @aliases costiris.task
#' @references See \code{\link[datasets]{iris}}.
#'   The cost matrix was generated artificially following
#'
#'   Tu, H.-H. and Lin, H.-T. (2010), One-sided support vector regression for multiclass cost-sensitive classification.
#'   In ICML, J. Fürnkranz and T. Joachims, Eds., Omnipress, 1095--1102.
#' @keywords data
#' @docType data
NULL

#' Yeast multilabel classification task.
#'
#' Contains the task (\code{yeast.task}).
#'
#' @name yeast.task
#' @source \url{http://sourceforge.net/projects/mulan/files/datasets/yeast.rar}
#' @references Elisseeff, A., & Weston, J. (2001):
#' A kernel method for multi-labelled classification.
#' In Advances in neural information processing systems (pp. 681-687).
#' @keywords data
#' @docType data
NULL


#' Gunpoint functional data classification task.
#'
#' Contains the task (\code{gunpoint.task}).
#' You have to classify whether a person raises up a gun or just an empty hand.
#'
#' @name gunpoint.task
#' @references See Ratanamahatana, C. A. & Keogh. E. (2004). Everything you know
#'   about Dynamic Time Warping is Wrong. Proceedings of SIAM International
#'   Conference on Data Mining (SDM05), 506-510.
#' @keywords data
#' @docType data
NULL


#' FuelSubset functional data regression task.
#'
#' Contains the task (\code{fuelsubset.task}).
#' 2 functional covariates and 1 scalar covariate.
#' You have to predict the heat value of some fuel based on the
#' ultraviolet radiation spectrum and infrared ray radiation and one scalar
#' column called h2o.
#'
#' The features and grids are scaled in the same way as in \code{\link[FDboost]{FDboost}}.
#'
#' @name fuelsubset.task
#' @references See Brockhaus, S., Scheipl, F., Hothorn, T., & Greven, S. (2015). The functional linear array model. Statistical Modelling, 15(3), 279–300.
#' @keywords data
#' @docType data
NULL

#' Phoneme functional data multilabel classification task.
#'
#' Contains the task (\code{phoneme.task}).
#' The task contains a single functional covariate and 5 equally big classes (aa, ao, dcl, iy, sh).
#' The aim is to predict the class of the phoneme in the functional.
#' The dataset is contained in the package fda.usc.
#'
#' @name phoneme.task
#' @references
#'   F. Ferraty and P. Vieu (2003) "Curve discrimination: a nonparametric functional approach", Computational Statistics and Data Analysis, 44(1-2), 161-173.
#'   F. Ferraty and P. Vieu (2006) Nonparametric functional data analysis, New York: Springer.
#'   T. Hastie and R. Tibshirani and J. Friedman (2009) The elements of statistical learning: Data mining, inference and prediction, 2nd edn, New York: Springer.
#' @keywords data
#' @docType data
NULL
