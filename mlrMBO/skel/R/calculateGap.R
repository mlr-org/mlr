# calculates gap between best point and global optimum
calculateGap = function(design, global.opt, control) {
  best.y = if(control$minimize)
    min(design[, control$y.name])
  else
    max(design[, control$y.name])
  gap =  abs(best.y - global.opt)
}
