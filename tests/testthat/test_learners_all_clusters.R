context("learners_all_clusters")

test_that("learners work: cluster", {

  # clustering, response
  task = noclass.task
  lrns = mylist(task, create = TRUE)
  for (lrn in lrns) {
    # FIXME: remove this if DBscan runs stable
    if (!inherits(lrn, "cluster.DBScan")) {
      expect_output(print(lrn), lrn$id)
      m = train(lrn, task)
      p = predict(m, task)
      expect_true(!is.na(performance(p, task = task)))
    }
  }

  # clustering, prob
  task = subsetTask(noclass.task, subset = 1:20, features = getTaskFeatureNames(noclass.task)[1:2])
  lrns = mylist(task, properties = "prob")
  lrns = lapply(lrns$class, makeLearner, predict.type = "prob")
  lapply(lrns, function(lrn) {
    m = train(lrn, task)
    p = predict(m, task)
    getPredictionProbabilities(p)
    expect_true(!is.na(performance(p, task = task)))
  })
})
