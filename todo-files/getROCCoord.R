getROCCoord <- function(object, thresholds = 50L){
  library(checkmate)
  assertInt(thresholds)
  thres = seq(0, 1, len = thresholds)

  extractCoord = function(object){
    if(object$pred$task$type != "classif") {
      stop('Task must be of type classif')
    }
    if(object$pred$predict.type != "prob"){
      stop("Predict.type must be prob")
    }
    pos = object$pred$task.desc$positive
    dat = object$pred$data[, c(2L, 4L, 6:7)]
    names(dat)[2L] = "prob"
    getCoord = function(dat) {
      dat = lapply(split(dat, dat$iter), function(dat) {
        data.frame(thres,
        TPR = sapply(thres, function(i) with(dat, sum(prob <= i & truth == pos)/sum(truth == pos))),
        FPR = sapply(thres, function(i) with(dat, sum(prob <= i & truth != pos)/sum(truth != pos))))
      })
      dat = do.call(rbind, dat)
      aggregate(cbind(TPR, FPR) ~ thres, dat, mean)
    }
    coord = lapply(split(dat, dat$set), getCoord)
    coord = do.call(rbind, coord)
    rownames(coord) = NULL
    coord = data.frame(set =  rep(levels(dat$set), each = thresholds), coord)
    coord
  }

  if(inherits(object, "BenchmarkResult")) {
    task.names = names(object)
    learner.names = unname(lapply(object, names))
    df = data.frame(
      task = rep.int(task.names, viapply(learner.names, length)),
      learner = unlist(learner.names),
      stringsAsFactors = FALSE
    )
    coord = rowLapply(df, function(x) extractCoord(object[[x$task]][[x$learner]]))
    df2 = df[rep(seq_along(coord), sapply(coord, nrow)),]
    coord = cbind(df2, do.call(rbind, coord))
  } else {
    coord = extractCoord(object)
  }
  coord
}
