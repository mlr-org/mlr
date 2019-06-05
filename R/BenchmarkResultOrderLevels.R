# order levels of task.ids in a BenchmarkResult or a similar data.frame
# useful for plotting in ggplot2
# if order.tsks is NULL, just return the df
orderBMRTasks = function(bmr, df = NULL, order.tsks) {
  if (is.null(df)) {
    df = as.data.frame(bmr)
  }
  if (!is.null(order.tsks)) {
    assertCharacter(order.tsks, len = length(getBMRTaskIds(bmr)))
    assertSetEqual(order.tsks, getBMRTaskIds(bmr), ordered = FALSE)
    df$task.id = factor(df$task.id, order.tsks)
  }
  return(df)
}

# order levels of learner.ids of a BenchmarkResult or similar data.frame
# useful for plotting in ggplot2
# if order.tsks is NULL, just return the df
orderBMRLrns = function(bmr, df = NULL, order.lrns) {
  if (is.null(df)) {
    df = as.data.frame(bmr)
  }
  if (!is.null(order.lrns)) {
    assertCharacter(order.lrns, len = length(getBMRLearnerIds(bmr)))
    assertSetEqual(order.lrns, getBMRLearnerIds(bmr), ordered = FALSE)
    df$learner.id = factor(df$learner.id, order.lrns)
  }
  return(df)
}
