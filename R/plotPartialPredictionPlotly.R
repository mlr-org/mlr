plotPartialPredictionPlotly = function(obj, p = 1) {
  assertClass(obj, "PartialPredictionData")
  if (length(obj$features) %nin% c(2L, 3L) & obj$interaction)
    stop("generatePartialPredictionPlotly must be called with two or three features to use this argument!")
  if (!obj$interaction)
    stop("generatePartialPredictionData was called with interaction = FALSE!")
  
  features = obj$features
  
  if (p != 1) {
    assertNumber(p, lower = 0, upper = 1, finite = TRUE)
    if (!obj$individual)
      stop("generatePartialPredictionData must be called with individual = TRUE to use this argument!")
    rows = unique(obj$data$idx)
    id = sample(rows, size = floor(p * length(rows)))
    obj$data = obj$data[which(obj$data$idx %in% id), ]
  }
  
  if (obj$task.desc$type %in% c("regr", "classif"))
    target = obj$task.desc$target
  else
    stop("generatePartialPredictionPlotly support currently only 'regression' and 'classification' tasks!")
  
  x1n = features[1]
  x2n = features[2]
  if (length(features) == 3L)
    x3n = features[3]
  
  
  require(plotly)
  
  if (obj$task.desc$type == "regr") {
    grid.dcast = reshape2::dcast(obj$data, as.formula(paste(x1n, x2n, sep = "~")), value.var = target)
    grid.3d = list(x = grid.dcast[,1],
                   y = as.numeric(colnames(grid.dcast)[-1]),
                   z = t(as.matrix(grid.dcast[,-1])))
    
    plt = plot_ly(data = grid.3d, x = x, y = y, z = z,
                  type = "surface")
    
    plt = plt %>% layout(scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                      yaxis = list(title = paste("y: ", x2n, sep = "")), 
                                      zaxis = list(title = paste("z: ", target, sep = ""))))
  }
  
  if (obj$task.desc$type == "classif") {
    grid.dcast = reshape2::dcast(obj$data, as.formula(paste(x1n, x2n, sep = "~")), value.var = "Probability")
    grid.3d = list(x = grid.dcast[,1],
                   y = as.numeric(colnames(grid.dcast)[-1]),
                   z = t(as.matrix(grid.dcast[,-1])))
    
    plt = plot_ly(data = grid.3d, x = x, y = y, z = z,
                  type = "surface")
    plt = plt %>% layout(scene = list(xaxis = list(title = paste("x: ", x1n, sep = "")),
                                      yaxis = list(title = paste("y: ", x2n, sep = "")), 
                                      zaxis = list(title = "Probability")))
  }
  
  plt
}