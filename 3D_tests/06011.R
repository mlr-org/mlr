if (taskdim == 3L) {
  if (show.point) {
    if (greyscale) {
      cols = RColorBrewer::brewer.pal(nlevels(data$Species) + 1, "Greys")[-1]
      data$.cols = as.character(factor(as.numeric(data[, target]), labels = cols))
      data[data$.err == T, "cols"] = "black"
      
      p = plot_ly(data = data, x = get(x1n), y = get(x2n), z = get(x3n), 
                  type = "scatter3d", mode = "markers", symbol = data[, target], 
                  marker = list(size = pointsize, opacity = alpha, color = .cols), 
                  text = "Input Data", legendgroup = "Input Data", 
                  showlegend = show.point.legend)
      
      if (show.bounding) {
        index = NULL
        for (i in 1:c(nrow(pred.grid$data) - 1)) {
          if (pred.grid$data[i, "response"] != pred.grid$data[i + 1, "response"] && (i %% (gridsize / 5)) != 0) {
            index = append(index, i)
          }
        }
        index = index[!duplicated(index)]
        
        p = add_trace(p, data = grid, x = get(x1n), y = get(x2n), z = get(x3n),
                      type = "mesh3d", mode = "markers", opacity = bounding.alpha,
                      color = grid[, target],
                      showlegend = show.bounding.legend, alphahull = 0,
                      text = "Bounding Point", legendgroup = "Bounding Point")
      }
    } else {
      cols = RColorBrewer::brewer.pal(nlevels(d$Species) + 1, "Greys")[-1]
      data$.cols = as.character(factor(as.numeric(data[, target]), labels = cols))
      data[data$.err == T, "cols"] = "black"
      
      p = plot_ly(data = data, x = get(x1n), y = get(x2n), z = get(x3n), 
                  type = "scatter3d", mode = "markers", symbol = data[, target], 
                  marker = list(size = pointsize, opacity = alpha, color = .cols), 
                  text = "Input Data", legendgroup = "Input Data", 
                  showlegend = show.point.legend)
    }
    
    p = p %>% layout(title = title,
                     scene = list(xaxis = list(title = paste("x: ", x1n)),
                                  yaxis = list(title = paste("y: ", x2n)),
                                  zaxis = list(title = paste("z: ", x3n))))
    
  }
}


if(taskdim == 3L) {
  if(show.point) {
    if(greyscale) {
      # point
      if(show.bounding){
        # bounding
      }
      
    } else {
      # point
      if(show.bounding){
        # bounding
      }
    }
  } else {
    if(greyscale){
      # bounding
    } else {
      # bounding
    }
  }
}
