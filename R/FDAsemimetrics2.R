#' # implement as S3 class for fd, fdata and matrix
#' # original implementation does not work for
#' # polygonal
#' # exponential
#' # monomial
#' # constant
#'
#'
#'
#' #' @title Proximities between functional Data.
#' #'
#' #' @description Mainly for internal use. Compute proximities between functional data objects.
#' #'   Aproximates semi-metric distances for functional data of class \code{fdata}
#' #'   or \code{fd}.
#' #'   The function \code{my.semimetric.basis} does exactly the same as
#' #'   \code{\link[fda.usc]{semimetric.basis}} and works as a check for the framework of
#' #'   newly implemented semi-metrics.
#' #'
#' #' @param fdata1 [\code{fdata(1)} or \code{fd(1)}]\cr
#' #'   Functional data 1
#' #' @param fdata2 [\code{fdata(1)} or \code{fd(1)}]\cr
#' #'   Functional data 2
#' #' @param nderiv [\code{integer(1)}]\cr
#' #'   Order of derivation used in \code{\link[fda.usc]{deriv.fd}}
#' #' @param type.basis1 [\code{character(1)}]\cr
#' #'   see \code{\link[fda.usc]{create.basis}}, default is "bspline". Warning,
#' #'   the original does not seem to work for \code{type.basis %in%
#' #'   c("polygonal", "exponential", "monomial", "constant")}
#' #' @param nbasis1 [\code{integer(1)}]\cr
#' #'   Number of basis for \code{fdata1}
#' #' @param type.basis2 [\code{character(1)}]\cr
#' #'   see \code{\link[fda]{create.basis}}, default is \code{type.basis1}
#' #' @param nbasis2 [\code{integer(1)}]\cr
#' #'   Number of basis for \code{fdata2}, default is \code{nbasis1}
#' #' @param ... [any]\cr
#' #'   Further arguments passed to and from arguments. Additional arguments
#' #'   to \code{\link[fda]{create.basis}}.
#' #' @return [\code{matrix}]\cr Returns a proximities matrix with dimensions
#' #'   \code{nrow(fdata.1)} x \code{nrow(fdata.2)}
#' #' @import fda.usc
#' #' @export
#' semimetric.mlr = function (fdata1, fdata2 = fdata1, nderiv = 0L,
#'                            evenly.spaced = FALSE,
#'                            derived = FALSE,
#'                            type.basis1 = NULL, nbasis1 = NULL,
#'                            type.basis2 = type.basis1, nbasis2 = NULL,
#'                            method = "basis", ...) {
#'   UseMethod("semimetric.mlr")
#' }
#'
#' #' @export
#' semimetric.mlr.fd = function (fdata1, fdata2 = fdata1, nderiv = 0L,
#'                               derived = FALSE,
#'                               method = "basis", ...) {
#'   requirePackages(c("fda.usc", "fda"))
#'   # input checking
#'   assertClass(fdata1, "fd")
#'   assertClass(fdata2, "fd")
#'   assertIntegerish(nderiv, len = 1)
#'
#'   # respace evenly so the new observations are observed on a regular grid
#'   r = fdata1$basis$rangeval
#'   tteval = seq(r[1], r[2], len = length(fdata1$fdnames$time))
#'
#'   # derive if required
#'   if(!derived) {
#'     fdata1 = fda::deriv.fd(fdata1, nderiv)
#'     fdata2 = fda::deriv.fd(fdata2, nderiv)
#'   }
#'   fd1 = fda.usc::fdata(t(fda::eval.fd(tteval, fdata1)), tteval, r)
#'   fd2 = fda.usc::fdata(t(fda::eval.fd(tteval, fdata2)), tteval, r)
#'
#'   return(semimetric.mlr.fdata(fdata1 = fd1, fdata2 = fd2,
#'                               nderiv = nderiv,
#'                               method = method,
#'                               evenly.spaced = TRUE, derived = TRUE,
#'                               ...))
#' }
#'
#'
#'
#' #' @export
#' semimetric.mlr.fdata = function(fdata1, fdata2 = fdata1,
#'                                 nderiv = 0L,
#'                                 method = "basis",
#'                                 evenly.spaced = NULL,
#'                                 derived = FALSE,
#'                                 type.basis1 = NULL, nbasis1 = NULL,
#'                                 type.basis2 = type.basis1, nbasis2 = NULL,
#'                                 ...) {
#'   requirePackages(c("fda.usc", "fda"))
#'   # input checking
#'   assertClass(fdata1, "fdata")
#'   assertClass(fdata2, "fdata")
#'   if (any(fda.usc::is.na.fdata(fdata1)))
#'     stop("fdata1 contain curves with some NA value \n")
#'   if (any(fda.usc::is.na.fdata(fdata1)))
#'     stop("fdata2 contain curves with some NA value \n")
#'
#'   assertIntegerish(nderiv, len = 1L)
#'   assertFlag(evenly.spaced, null.ok = TRUE)
#'   assertFlag(derived)
#'
#'
#'
#'   if(is.null(evenly.spaced)) {
#'     # helper function to check if functional data is observed on a regular grid
#'     is.evenly.spaced = function(fdata) {
#'       all.equal(fdata$argvals, seq(fdata$rangeval[1], fdata$rangeval[2],
#'                                    length.out = length(fdata$argvals)))
#'     }
#'     evenly.spaced = is.evenly.spaced(fdata1) & is.evenly.spaced(fdata2)
#'   }
#'
#'   # this is kind of a recursive function call
#'   # if the data is of type fdata, evenly spaced and does not need to be derived,
#'   # the distances are computed directly.
#'   # else the data is transformed to be of class fd, then derived, evenly spaced
#'   # and transformed back to data type fdata. Then the function for
#'   # semimetric.mlr.fdata can be used with evenly spaced data that does not need
#'   # derivation.
#'   if(evenly.spaced & (nderiv == 0L | derived)) {
#'     # Return the semimetric that is asked for
#'     distmat = computeSemimetric(mat1 = fdata1$data, mat2 = fdata2$data,
#'                                 method = method, ...)
#'     attr(distmat, "call") <- "semimetric.mlr"
#'     attr(distmat, "par.metric") <- list(nderiv = nderiv,
#'                                         method = method,
#'                                         type.basis1 = type.basis1, nbasis1 = nbasis1,
#'                                         type.basis2 = type.basis2, nbasis2 = nbasis2)
#'
#'     return(distmat)
#'   } else {
#'     # transform to fd format to derive and respace evenly
#'     # input checking type.basis
#'     if(is.null(type.basis1))
#'       type.basis1 = "bspline"
#'     if(is.null(type.basis2))
#'       type.basis2 = type.basis1
#'     assertCharacter(type.basis1)
#'
#'     basis.choices = c("bspline", "polygonal", "fourier", "exponential",
#'                       "monomial", "constant", "pc", "pls", "power")
#'     assertChoice(type.basis1, choices = basis.choices)
#'     assertCharacter(type.basis2)
#'     assertChoice(type.basis2, choices = basis.choices)
#'
#'     # input checking nbasis
#'     np = ncol(fdata1)
#'     if (is.null(nbasis1)) {
#'       nbasis1 = ifelse(floor(np/3) > floor((np - nderiv - 4)/2),
#'                        floor((np - nderiv - 4)/2), floor(np/3))
#'     }
#'     if (is.null(nbasis2))
#'       nbasis2 = nbasis1
#'
#'     assertIntegerish(nbasis1, len = 1)
#'     assertIntegerish(nbasis2, len = 1)
#'
#'     tt <- fdata1[["argvals"]]
#'     rtt <- fdata1[["rangeval"]]
#'
#'     # get all arguments from the function call
#'     Call <- as.list(match.call())
#'     args = c(Call, list("nbasis" = nbasis1,"argvals" = tt, "rangeval" = rtt))
#'
#'
#'     # create basis representations from fda package
#'     base1 <- paste("create.", type.basis1, ".basis", sep = "")
#'     args.basis1 = args[names(args) %in% names(formals(base1))]
#'     b1.1 <- do.call(base1, args.basis1)
#'
#'
#'     base2 <- paste("create.", type.basis2, ".basis", sep = "")
#'     args.basis2 = args[names(args) %in% names(formals(base2))]
#'     b1.2 <- do.call(base2, args.basis2)
#'
#'     # convert to fd class to derive and resample evenly
#'     # fd1.1 <- fda::Data2fd(argvals = tt, y = t(fdata1$data))
#'     # fd1.2 <- fda::Data2fd(argvals = tt, y = t(fdata2$data))
#'     fd1.1 <- fda::Data2fd(argvals = tt, y = t(fdata1$data), basisobj = b1.1)
#'     fd1.2 <- fda::Data2fd(argvals = tt, y = t(fdata2$data), basisobj = b1.2)
#'
#'     return(semimetric.mlr.fd(fdata1 = fd1.1, fdata2 = fd1.2,
#'                              nderiv = nderiv, method = method,
#'                              derived = derived,
#'                              type.basis1 = type.basis1, nbasis1 = nbasis1,
#'                              type.basis2 = type.basis2, nbasis2 = nbasis2,...))
#'   }
#' }
#'
#' # Wrapper for \code{\link{computeSemimetric}}.
#' #' @export
#' semimetric.mlr.matrix = function (fdata1, fdata2 = fdata1, nderiv = 0L,
#'                                   method = "basis",
#'                                   evenly.spaced = TRUE, # maybe set to NULL and check
#'                                   derived = FALSE,
#'                                   type.basis1 = NULL, nbasis1 = NULL,
#'                                   type.basis2 = type.basis1, nbasis2 = NULL,
#'                                   ...) {
#'   requirePackages(c("fda.usc", "fda"))
#'   # input checking
#'   assertClass(fdata1, "matrix")
#'   assertClass(fdata2, "matrix")
#'   if (any(is.na(fdata1)))
#'     stop("fdata1 contain curves with some NA value \n")
#'   if (any(is.na(fdata1)))
#'     stop("fdata2 contain curves with some NA value \n")
#'   assertIntegerish(nderiv, len = 1L)
#'
#'   # data in matrix form is assumed to be observed on a regular grid
#'   if(is.null(evenly.spaced)) {
#'     evenly.spaced = TRUE
#'   }
#'   assertFlag(evenly.spaced)
#'   if(!evenly.spaced) {
#'     stop("semimetric.mlr.matrix() expects evenly spaced observations (evenly.spaced = TRUE).
#'          Please use semimetric.mlr.fdata().")
#'   }
#'   assertFlag(derived)
#'
#'   if(evenly.spaced & (nderiv == 0L | derived)) {
#'     # Return the semimetric that is asked for
#'     distmat = computeSemimetric(mat1 = fdata1, mat2 = fdata2,
#'                                 method = method, ...)
#'     attr(distmat, "call") <- "semimetric.mlr"
#'     attr(distmat, "par.metric") <- list(nderiv = nderiv,
#'                                         method = method,
#'                                         type.basis1 = type.basis1, nbasis1 = nbasis1,
#'                                         type.basis2 = type.basis2, nbasis2 = nbasis2)
#'
#'     return(distmat)
#'   } else {
#'     return(semimetric.mlr.fdata(fdata1 = fda.usc::fdata(fdata1),
#'                                 fdata2 = fda.usc::fdata(fdata2),
#'                                 nderiv = nderiv,
#'                                 method = method,
#'                                 evenly.spaced = TRUE,
#'                                 derived = FALSE,
#'                                 type.basis1 = type.basis1, nbasis1 = nbasis1,
#'                                 type.basis2 = type.basis2, nbasis2 = nbasis2,
#'                                 ...))
#'   }
#' }
#'
#'
#' # mat1 = matrix(1:15, ncol = 3, byrow = TRUE)
#' # mat2 = matrix(11:25, ncol = 3, byrow = TRUE)
#' #' @export
#' computeSemimetric = function(mat1, mat2,
#'                              method = "basis", ...) {
#'   requirePackages("proxy")
#'   proxy.set = unlist(summary(proxy::pr_DB)$names)
#'   assertChoice(method, choices = c("basis", proxy.set))
#'
#'   # if(method == "basis") {
#'   #   return(fda.usc::metric.lp(mat1, mat2, ...))
#'   # }
#'   if(method == "basis") {
#'     return(as.matrix(proxy::dist(mat1, mat2, method = "Lp", p = lp, ...)))
#'   }
#'
#'   if(method %in% proxy.set) {
#'     return(as.matrix(proxy::dist(mat1, mat2, method = method, ...)))
#'   }
#'
#' }
#'
#' # run time comparison
#' # system.time({p1 = proxy::dist(phoneme[["learn"]]$data, method = "Minkowski",
#' #                               p = lp, diag = TRUE, upper = TRUE)})
#' # p1mat = as.matrix(p1)
#' #
#' # system.time({p2 = fda.usc::metric.lp(phoneme[["learn"]]$data, lp = lp)})
#' # p2mat = as.matrix(p2, ncol = 250)
#' #
#' # attributes(p1mat) = NULL
#' # attributes(p2mat) = NULL
#' # all.equal(p1mat, p2mat)
#'
#'
#' # dist1 = fda.usc::metric.lp(phoneme[["learn"]]$data, lp = 2)
#' # dist2 = fda.usc::metric.lp(phoneme[["learn"]], lp = 2)
#' #
#' # all.equal(dist1, dist2)
#'
#'
