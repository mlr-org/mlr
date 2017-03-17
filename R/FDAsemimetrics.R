# my.semimetric.basis(fdata1 = data.fdclass)

# fdata1 = data.fdclass

# implement as S3 class for fd or fdata
# original implementation does not work for
# polygonal
# exponential
# monomial
# constant



#' @title Proximities between functional Data.
#'
#' @description Mainly for internal use. Compute proximities between functional data objects.
#'   Aproximates semi-metric distances for functional data of class \code{fdata}
#'   or \code{fd}.
#'   The function \code{my.semimetric.basis} does exactly the same as
#'   \code{\link[fda.usc]{semimetric.basis}} and works as a check for the framework of
#'   newly implemented semi-metrics.
#'
#' @param fdata1 [\code{fdata(1)} or \code{fd(1)}]\cr
#'   Functional data 1
#' @param fdata2 [\code{fdata(1)} or \code{fd(1)}]\cr
#'   Functional data 2
#' @param nderiv [\code{integer(1)}]\cr
#'   Order of derivation used in \code{\link[fda.usc]{deriv.fd}}
#' @param type.basis1 [\code{character(1)}]\cr
#'   see \code{\link[fda.usc]{create.basis}}, default is "bspline". Warning,
#'   the original does not seem to work for \code{type.basis %in%
#'   c("polygonal", "exponential", "monomial", "constant")}
#'   #' @param nbasis1 [\code{integer(1)}]\cr
#'   Number of basis for \code{fdata1}
#' @param type.basis2 [\code{character(1)}]\cr
#'   see \code{\link[fda]{create.basis}}, default is \code{type.basis1}
#' @param nbasis2 [\code{integer(1)}]\cr
#'   Number of basis for \code{fdata2}, default is \code{nbasis1}
#' @param ... [any]\cr
#'   Further arguments passed to and from arguments. Additional arguments
#'   to \code{\link[fda]{create.basis}}.
#' @return [\code{matrix}]\cr Returns a proximities matrix with dimensions
#'   \code{nrow(fdata.1)} x \code{nrow(fdata.2)}
#' @import fda.usc # throws error but then the rest of the code works
#' @name semimetrics.mlr
#' @rdname semimetrics.mlr
NULL

#' @rdname semimetrics.mlr
#' @export
semimetric.mlr.basis = function (fdata1, fdata2 = fdata1, nderiv = 0L,
                                type.basis1 = NULL, nbasis1 = NULL,
                                type.basis2 = type.basis1, nbasis2 = NULL, ...) {
  UseMethod("semimetric.mlr.basis")
}


#' @export
semimetric.mlr.basis.fd = function (fdata1, fdata2 = fdata1, nderiv = 0L,
                                   # type.basis1 = NULL, nbasis1 = NULL,
                                   # type.basis2 = type.basis1, nbasis2 = NULL,
                                   ...) {
  requirePackages(c("fda.usc", "fda"))
  # input checking
  assertClass(fdata1, "fd")
  assertClass(fdata2, "fd")

  assertIntegerish(nderiv, len = 1)

  # resample evenly
  r = fdata1$basis$rangeval
  tteval = seq(r[1], r[2], len = length(fdata1$fdnames$time))
  df1 = fda::deriv.fd(fdata1, nderiv)
  df2 = fda::deriv.fd(fdata2, nderiv)
  fd1 = fda.usc::fdata(t(eval.fd(tteval, df1)), tteval, r)
  fd2 = fda.usc::fdata(t(eval.fd(tteval, df2)), tteval, r)

  # Calculate the actual semimetric
  mdist = fda.usc::metric.lp(fd1, fd2, ...)
  # print(attributes(mdist))
  # attr(mdist, "call") = "semimetric.basis"
  # attr(mdist, "par.metric") = list(nderiv = nderiv)
}

#' @export
semimetric.mlr.basis.fdata = function (fdata1, fdata2 = fdata1, nderiv = 0L,
                                      type.basis1 = NULL, nbasis1 = NULL,
                                      type.basis2 = type.basis1, nbasis2 = NULL, ...) {
  requirePackages(c("fda.usc", "fda"))
  # input checking
  assertClass(fdata1, "fdata")
  assertClass(fdata2, "fdata")
  if (any(fda.usc::is.na.fdata(fdata1)))
    stop("fdata1 contain curves with some NA value \n")
  if (any(fda.usc::is.na.fdata(fdata1)))
    stop("fdata2 contain curves with some NA value \n")

  assertIntegerish(nderiv, len = 1)

  # input checking type.basis
  if(is.null(type.basis1))
    type.basis1 = "bspline"
  if(is.null(type.basis2))
    type.basis2 = type.basis1
  assertCharacter(type.basis1)

  basis.choices = c("bspline", "polygonal", "fourier", "exponential",
                    "monomial", "constant", "pc", "pls", "power")
  assertChoice(type.basis1, choices = basis.choices)
  assertCharacter(type.basis2)
  assertChoice(type.basis2, choices = basis.choices)

  # input checking nbasis
  np = ncol(fdata1)
  if (is.null(nbasis1)) {
    nbasis1 = ifelse(floor(np/3) > floor((np - nderiv -
                                            4)/2), floor((np - nderiv - 4)/2), floor(np/3))
  }
  if (is.null(nbasis2))
    nbasis2 = nbasis1

  assertIntegerish(nbasis1, len = 1)
  assertIntegerish(nbasis2, len = 1)

  tt <- fdata1[["argvals"]]
  rtt <- fdata1[["rangeval"]]

  # get all arguments from the function call
  Call <- as.list(match.call())
  args = c(Call, list("nbasis" = nbasis1, "rangeval" = rtt))


  # create basis representations from fda package
  base1 <- paste("create.", type.basis1, ".basis", sep = "")
  args.basis1 = args[names(args) %in% names(formals(base1))]
  b1.1 <- do.call(base1, args.basis1)


  base2 <- paste("create.", type.basis1, ".basis", sep = "")
  args.basis2 = args[names(args) %in% names(formals(base2))]
  b1.2 <- do.call(base2, args.basis2)

  # convert to fd class to derive and resample evenly
  fd1.1 <- fda::Data2fd(argvals = tt, y = t(fdata1$data), basisobj = b1.1)
  fd1.2 <- fda::Data2fd(argvals = tt, y = t(fdata2$data), basisobj = b1.2)

  # compute the actual distance
  mdist = semimetric.mlr.basis.fd(fd1.1, fd1.2, nderiv = nderiv)

  attr(mdist, "call") <- as.character(Call[[1]])
  attr(mdist, "par.metric") <- list(nderiv = nderiv,
                                    type.basis1 = type.basis1, nbasis1 = nbasis1,
                                    type.basis2 = type.basis2, nbasis2 = nbasis2)


  return(mdist)
}







#' @rdname semimetrics.mlr
#' @export
semimetric.mlr = function (fdata1, fdata2 = fdata1, nderiv = 0L,
                                type.basis1 = NULL, nbasis1 = NULL,
                                type.basis2 = type.basis1, nbasis2 = NULL, ...) {
  UseMethod("semimetric.mlr")
}

#' @export
semimetric.mlr.fd = function (fdata1, fdata2 = fdata1, nderiv = 0L,
                                   # type.basis1 = NULL, nbasis1 = NULL,
                                   # type.basis2 = type.basis1, nbasis2 = NULL,
                                   ...) {
  requirePackages(c("fda.usc", "fda"))
  # input checking
  assertClass(fdata1, "fd")
  assertClass(fdata2, "fd")

  assertIntegerish(nderiv, len = 1)

  # resample evenly
  r = fdata1$basis$rangeval
  tteval = seq(r[1], r[2], len = length(fdata1$fdnames$time))
  df1 = fda::deriv.fd(fdata1, nderiv)
  df2 = fda::deriv.fd(fdata2, nderiv)
  fd1 = fda.usc::fdata(t(eval.fd(tteval, df1)), tteval, r)
  fd2 = fda.usc::fdata(t(eval.fd(tteval, df2)), tteval, r)

  # Calculate the actual semimetric
  mdist = fda.usc::metric.lp(fd1, fd2, ...)
}

#' @export
semimetric.mlr.fdata = function (fdata1, fdata2 = fdata1, nderiv = 0L,
                                      type.basis1 = NULL, nbasis1 = NULL,
                                      type.basis2 = type.basis1, nbasis2 = NULL, ...) {
  requirePackages(c("fda.usc", "fda"))
  # input checking
  assertClass(fdata1, "fdata")
  assertClass(fdata2, "fdata")
  if (any(fda.usc::is.na.fdata(fdata1)))
    stop("fdata1 contain curves with some NA value \n")
  if (any(fda.usc::is.na.fdata(fdata1)))
    stop("fdata2 contain curves with some NA value \n")

  assertIntegerish(nderiv, len = 1)

  # input checking type.basis
  if(is.null(type.basis1))
    type.basis1 = "bspline"
  if(is.null(type.basis2))
    type.basis2 = type.basis1
  assertCharacter(type.basis1)

  basis.choices = c("bspline", "polygonal", "fourier", "exponential",
                    "monomial", "constant", "pc", "pls", "power")
  assertChoice(type.basis1, choices = basis.choices)
  assertCharacter(type.basis2)
  assertChoice(type.basis2, choices = basis.choices)

  # input checking nbasis
  np = ncol(fdata1)
  if (is.null(nbasis1)) {
    nbasis1 = ifelse(floor(np/3) > floor((np - nderiv -
                                            4)/2), floor((np - nderiv - 4)/2), floor(np/3))
  }
  if (is.null(nbasis2))
    nbasis2 = nbasis1

  assertIntegerish(nbasis1, len = 1)
  assertIntegerish(nbasis2, len = 1)

  tt <- fdata1[["argvals"]]
  rtt <- fdata1[["rangeval"]]

  # get all arguments from the function call
  Call <- as.list(match.call())
  args = c(Call, list("nbasis" = nbasis1, "rangeval" = rtt))


  # create basis representations
  base1 <- paste("fda::create.", type.basis1, ".basis", sep = "")
  args.basis1 = args[names(args) %in% names(formals(base1))]
  b1.1 <- do.call(base1, args.basis1)


  base2 <- paste("fda::create.", type.basis1, ".basis", sep = "")
  args.basis2 = args[names(args) %in% names(formals(base2))]
  b1.2 <- do.call(base2, args.basis2)

  # convert to fd class to derive and resample evenly
  fd1.1 <- fda::Data2fd(argvals = tt, y = t(fdata1$data), basisobj = b1.1)
  fd1.2 <- fda::Data2fd(argvals = tt, y = t(fdata2$data), basisobj = b1.2)

  # compute the actual distance
  mdist = semimetric.mlr.basis.fd(fd1.1, fd1.2, nderiv = nderiv)

  return(mdist)
}
