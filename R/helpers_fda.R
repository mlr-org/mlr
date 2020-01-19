# Convert fd.features list to list of column indices and check for consitency.
fdFeatsToColumnIndex = function(df, fd.features = NULL, exclude.cols = NULL) {

  # If the data.frame already contains matricies, keep them
  fd.mats = which(vcapply(df, function(x) class(x)[1L]) == "matrix")

  # If fd.features is NULL, all numerics are a single functional feature
  # Already existing matricies are not converted
  if (is.null(fd.features)) {
    fd.features = list("fd1" = setdiff(which(vlapply(df, is.numeric)), c(exclude.cols, fd.mats)))
  }

  # Return the column index and check if indices/names refer to columns
  lapply(fd.features, function(fd.feature) {
    if (is.character(fd.feature)) {
      assertSubset(fd.feature, colnames(df), empty.ok = FALSE)
      setdiff(which(colnames(df) %in% fd.feature), exclude.cols)
    } else {
      assertSubset(fd.feature, seq_len(ncol(df)))
      setdiff(fd.feature, exclude.cols)
    }
  })
}

# Convert a data.frame containing functional features to a data.frame containing
# them as numerics.
functionalToNormalData = function(df) {
  if (hasFunctionalFeatures(df)) {
    df = do.call(data.frame, as.list(df))
    message("Functional features have been converted to numerics")
  }
  return(df)
}


# Helper function that checks functional data columns for consistency
# Takes a data.frame and a functional data column
# Returns the functional matrix-column
checkFDCols = function(data, col) {
  assertClass(data, "data.frame")
  assertChoice(col, choices = colnames(data))
  data = as.matrix(data[, col, drop = FALSE])
  assertNumeric(data)
  return(data)
}

# created by `classiFunc::metric.choices`
# we cannot use the raw function here as otherwise pkg classiFunc would act like
# a depenency of mlr (because this here is called during loading) the same goes
# for the actual use of this object in the ParamSet of the classiFunc learners
# From time to time we should update this list (or if we experience errors)
# to have the latest settings
# last update: 01/2020
metric.choices = c(cosine1 = "cosine", cosine2 = "angular", eJaccard1 = "eJaccard",
  eJaccard2 = "extended_Jaccard", eDice1 = "eDice", eDice2 = "extended_Dice",
  eDice3 = "eSorensen", correlation = "correlation", Euclidean1 = "Euclidean",
  Euclidean2 = "L2", Mahalanobis = "Mahalanobis", Bhjattacharyya = "Bhjattacharyya",
  Manhattan1 = "Manhattan", Manhattan2 = "City-Block", Manhattan3 = "L1",
  Manhattan4 = "taxi", supremum1 = "supremum", supremum2 = "max",
  supremum3 = "maximum", supremum4 = "Tschebyscheff", supremum5 = "Chebyshev",
  Minkowski1 = "Minkowski", Minkowski2 = "Lp", Canberra = "Canberra",
  Wave1 = "Wave", Wave2 = "Hedges", divergence = "divergence",
  Kullback1 = "Kullback", Kullback2 = "Leibler", Bray1 = "Bray",
  Bray2 = "Curtis", Soergel = "Soergel", Podani1 = "Podani", Podani2 = "discordance",
  Chord = "Chord", Geodesic = "Geodesic", Whittaker = "Whittaker",
  Hellinger = "Hellinger", fJaccard1 = "fJaccard", fJaccard2 = "fuzzy_Jaccard",
  "shortEuclidean", "mean", "relAreas", "jump", "globMax", "globMin",
  "points", "custom.metric", "amplitudeDistance", "phaseDistance",
  "FisherRao", "elasticMetric", "elasticDistance", "dtwPath", "rucrdtw",
  "rucred")
# created by `classiFunc::metric.choices`
kernel.choices = c("Ker.norm", "Ker.cos", "Ker.epa", "Ker.tri", "Ker.quar", "Ker.unif",
  "AKer.norm", "AKer.cos", "AKer.epa", "AKer.tri", "AKer.quar",
  "AKer.unif", "custom.ker")
