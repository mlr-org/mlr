# order levels of task.ids of a BenchmarkResult
# usefull for plotting in ggplot2

orderBMRTasks = function(bmr, df = NULL, order.tsks) {
  
  assertClass(bmr, "BenchmarkResult")
  assertVector(order.tsks, len = length(getBMRTaskIds(bmr)))
  
  if (is.null(df)) {
    df = as.data.frame(bmr)
  } else {
    assertClass(df, "data.frame")
  }
  if (is.numeric(order.tsks)) {
    order.tsks = getBMRTaskIds(bmr)[order.tsks]
  } else {
    assertCharacter(order.tsks)
  }
  assertSetEqual(order.tsks, getBMRTaskIds(bmr), ordered = FALSE) 
  
  # change levels
  df$task.id = factor(df$task.id, order.tsks)
  return(df)
}



# order levels of learner.ids of a BenchmarkResult
# usefull for plotting in ggplot2
orderBMRLrns = function(bmr, df = NULL, order.lrns){
  
  assertClass(bmr, "BenchmarkResult")
  assertVector(order.lrns, len = length(getBMRLearnerIds(bmr)))
  
  # create df and/or getLearnerIds
  if (is.null(df)) {
    df = as.data.frame(bmr)
  } else {
    assertClass(df, "data.frame")
  }
  if (is.numeric(order.lrns)) {
    order.lrns = getBMRLearnerIds(bmr)[order.lrns]
  } else {
    assertCharacter(order.lrns)
  }
  
  # change levels
  assertSetEqual(order.lrns, getBMRLearnerIds(bmr), ordered = FALSE) 
  df$learner.id = factor(df$learner.id, order.lrns)
  return(df)
}
