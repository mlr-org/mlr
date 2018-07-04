#' Iris classification task.
#'
#' Contains the task (`iris.task`).
#'
#' @name iris.task
#' @references See [datasets::iris].
#' @keywords data
#' @docType data
NULL

#' Sonar classification task.
#'
#' Contains the task (`sonar.task`).
#'
#' @name sonar.task
#' @references See [mlbench::Sonar].
#' @keywords data
#' @docType data
NULL

#' Wisconsin Breast Cancer classification task.
#'
#' Contains the task (`bc.task`).
#'
#' @name bc.task
#' @references See [mlbench::BreastCancer].
#'   The column `"Id"` and all incomplete cases have been removed from the task.
#' @keywords data
#' @docType data
NULL

#' PimaIndiansDiabetes classification task.
#'
#' Contains the task (`pid.task`).
#'
#' @name pid.task
#' @references See [mlbench::PimaIndiansDiabetes].
#'   Note that this is the uncorrected version from mlbench.
#' @keywords data
#' @docType data
NULL

#' Boston Housing regression task.
#'
#' Contains the task (`bh.task`).
#'
#' @name bh.task
#' @aliases bh.task
#' @references See [mlbench::BostonHousing].
#' @keywords data
#' @docType data
NULL

#' Wisonsin Prognostic Breast Cancer (WPBC) survival task.
#'
#' Contains the task (`wpbc.task`).
#'
#' @name wpbc.task
#' @aliases wpbc.task
#' @references See [TH.data::wpbc].
#'  Incomplete cases have been removed from the task.
#' @keywords data
#' @docType data
NULL

#' NCCTG Lung Cancer survival task.
#'
#' Contains the task (`lung.task`).
#'
#' @name lung.task
#' @aliases lung.task
#' @references See [survival::lung].
#'  Incomplete cases have been removed from the task.
#' @keywords data
#' @docType data
NULL

#' Motor Trend Car Road Tests clustering task.
#'
#' Contains the task (`mtcars.task`).
#'
#' @name mtcars.task
#' @aliases mtcars.task
#' @references See [datasets::mtcars].
#' @keywords data
#' @docType data
NULL

#' European Union Agricultural Workforces clustering task.
#'
#' Contains the task (`agri.task`).
#'
#' @name agri.task
#' @aliases agri.task
#' @references See [cluster::agriculture].
#' @keywords data
#' @docType data
NULL

#' Iris cost-sensitive classification task.
#'
#' Contains the task (`costiris.task`).
#'
#' @name costiris.task
#' @aliases costiris.task
#' @references See [datasets::iris].
#'   The cost matrix was generated artificially following
#'
#'   Tu, H.-H. and Lin, H.-T. (2010), One-sided support vector regression for multiclass cost-sensitive classification.
#'   In ICML, J. Fürnkranz and T. Joachims, Eds., Omnipress, 1095--1102.
#' @keywords data
#' @docType data
NULL

#' Yeast multilabel classification task.
#'
#' Contains the task (`yeast.task`).
#'
#' @name yeast.task
#' @source <https://archive.ics.uci.edu/ml/datasets/Yeast> (In long instead of wide format)
#' @references Elisseeff, A., & Weston, J. (2001):
#' A kernel method for multi-labelled classification.
#' In Advances in neural information processing systems (pp. 681-687).
#' @keywords data
#' @docType data
NULL

#' J. Muenchow's Ecuador landslide data set
#'
#' Data set created by Jannes Muenchow, University of Erlangen-Nuremberg,
#' Germany.
#' These data should be cited as Muenchow et al. (2012) (see reference below).
#' This publication also contains additional information on data collection and
#' the geomorphology of the area. The data set provded here is (a subset of) the
#' one from the 'natural' part of the RBSF area and corresponds to landslide
#' distribution in the year 2000.
#' @name spatial.task
#'
#' @keywords datasets
#'
#' @docType data
#'
#' @format a `data.frame` with point samples of landslide and
#' non-landslide locations in a study area in the Andes of southern Ecuador.
#'
#' @references Muenchow, J., Brenning, A., Richter, M., 2012. Geomorphic process
#' rates of landslides along a humidity gradient in the tropical Andes.
#' Geomorphology, 139-140: 271-284.
#'
#' Brenning, A., 2005. Spatial prediction models for landslide hazards:
#' review, comparison and evaluation.
#' Natural Hazards and Earth System Sciences, 5(6): 853-862.
NULL

#' Gunpoint functional data classification task.
#'
#' Contains the task (`gunpoint.task`).
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
#' Contains the task (`fuelsubset.task`).
#' 2 functional covariates and 1 scalar covariate.
#' You have to predict the heat value of some fuel based on the
#' ultraviolet radiation spectrum and infrared ray radiation and one scalar
#' column called h2o.
#'
#' The features and grids are scaled in the same way as in [FDboost::FDboost].
#'
#' @name fuelsubset.task
#' @references See Brockhaus, S., Scheipl, F., Hothorn, T., & Greven, S. (2015). The functional linear array model. Statistical Modelling, 15(3), 279–300.
#' @keywords data
#' @docType data
NULL

#' Phoneme functional data multilabel classification task.
#'
#' Contains the task (`phoneme.task`).
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

#' Spam classification task.
#'
#' Contains the task (`spam.task`).
#'
#' @name spam.task
#' @references See [kernlab::spam].
#' @keywords data
#' @docType data
NULL
