# Order Levels of task.ids of a BenchmarkResult
# Usefull for plotting in ggplot2
orderBMRTasks = function(bmr, df = NULL, order.Tsks) {
  
  # Assert correct input 
  assertClass(bmr, "BenchmarkResult")
  assertVector(order.Tsks, len = length(getBMRTaskIds(bmr)))
  
  
  if (is.null(df)) {
    df = as.data.frame(bmr)
  } else {
    assertClass(df, "data.frame")
  }
  if (is.numeric(order.Tsks)) {
    order.Tsks= getBMRTaskIds(bmr)[order.Tsks]
  } else {
    assertCharacter(order.Tsks)
  }
  assertSetEqual(order.Tsks, getBMRTaskIds(bmr), ordered = FALSE) 
  
  #Change levels
  df$task.id = factor(df$task.id, order.Tsks)
  return(df)
}



# Order Levels of learner.ids of a BenchmarkResult
# Usefull for plotting in ggplot2
orderBMRLrns = function(bmr,df=NULL,order.Lrns){
  
  # Assert correct input
  assertClass(bmr, "BenchmarkResult")
  assertVector(order.Lrns, len = length(getBMRLearnerIds(bmr)))
  
  # Create df and or getLearnerIds
  if (is.null(df)) {
    df = as.data.frame(bmr)
  } else {
    assertClass(df, "data.frame")
  }
  if (is.numeric(order.Lrns)) {
    order.Lrns = getBMRLearnerIds(bmr)[order.Lrns]
  } else {
    assertCharacter(order.Lrns)
  }
  
  # Change Levels
  assertSetEqual(order.Lrns, getBMRLearnerIds(bmr), ordered = FALSE) 
  df$learner.id = factor(df$learner.id, order.Lrns)
  return(df)
}
