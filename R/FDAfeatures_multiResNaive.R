# @param x [\code{numeric(n)}]\cr
# The input curve
getSegmentFeatures = function(x) {
  mean(x)
}
# @param x[\code{numeric(n)}]\cr
# The input curve
# @param res.level [\code{integer}]\cr
# The number of hierachy of resolutions 
# @param shift [\code{numeric}]\cr
# The overlapping proportion when slide the window for one step 
getCurveFeatures = function(x, res.level = 3, shift = 0.5) {
  m = length(x)
  start = 1L
  feats = numeric(0L)
  ssize = m  # initialize segment size to be the length of the curve
  for (rl in 1:res.level) {
    soffset = ceiling(shift * ssize)  # overlap distance
    messagef("reslev = %i, ssize = %i, soffset=%i", rl, ssize, soffset)
    sstart = 1L
    send = sstart + ssize - 1L  # end position
    while(send <= m) {  # until the segment reach the end
      # messagef("start, end: %i, %i", sstart, send)
      f = getSegmentFeatures(x[sstart:send])
      # print(f)
      feats = c(feats, f)  # append the feats from the last resolution hierachy
      sstart = sstart + soffset
      send = send + soffset
    }
    ssize = ceiling(ssize / 2)  # decrease the segment size
    if (ssize < 1L)
      break
  }
  return(feats)
}

#' @title multiresolution feature extraction
#'
#' @description The function extract the mean of a small segments of the curve and stack them as features. The segments length 
#' are set in a hierachy way so the features cover different resolution levels.
#' @param data[\code{dataframe}]\cr 
#' the input matrix
#' @param cuve.lens[\code{vector}]\cr
#' the subcurve length vector, suggest to sum up to the lenght of the curve
#' @param res.level[\code{integer}]\cr
#' the number of resolution hierachy
#' @param shift [\code{numeric}]\cr
#' The overlapping proportion when slide the window for one step
#' @return Returns a \code{matrix} object with each row containing the multi-resolution features
#' @export 
extractFDAMultiResFeatures = function(data, curve.lens, res.level = 3L, shift = 0.5) {
  checkmate::assert_matrix(data)
  n.obs = nrow(data)
  n.curves = length(curve.lens)
  feat.list = vector("list", n.obs)  # class(feat.list) = "list", vector(mode = "logical", length = 0)
  for (i in 1:n.obs) {  # traverse the number of observations
    # print(i)
    featvec = numeric(0L)
    for (j in 1:n.curves) {
      clen = curve.lens[j]  # the subcurve length
      sstart = ifelse(j == 1L, 1L, send + 1L)  # the start point of the sub curve, without overlap
      send = sstart + clen - 1L
      #messagef("curve start, end: %i, %i", sstart, send)
      f = getCurveFeatures(data[i, sstart:send], res.level = res.level, shift = shift)
      # print(f)
      featvec = c(featvec, f)
    }
    feat.list[[i]] = featvec  # put features from the ith instance into the list ith position
  }
  do.call(rbind, feat.list)  # creat a matrix by combining the row
}

# FIXME: I have commented out this block so as to pass the rcheck() command, will uncomment once the bug is fixed
# extractMultiResFeatures2 = function(data, curve.lens, res.level = 3, shift = 0.5) {
#   resmat = matrix(NA_real_, nrow = 1L, ncol = 100000L)
#   .Call(c_get_multires_curve_features, data[1L,,drop = FALSE], curve.lens, resmat, res.level, shift)
#   p = which.first(is.na(resmat[1L,])) - 1L
#   resmat = matrix(0, nrow = nrow(data), ncol = p)
#   .Call(c_get_multires_curve_features, data, curve.lens, resmat, res.level, shift)
#   return(resmat)
# }

