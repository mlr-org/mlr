
1. Hacking the class check:
R/checkLearnerBeforeTrain.R

-  if (td$type != learner$type) {
+  # FIXME: this seems a bit bad style, but the other option would be to somehow create
+  # a wrapper for normal classif and regr learners? more code for user and and a bit unintuitive...?
+  if (td$type != learner$type && !(td$type == "tsclassif" && learner$type == "classif")) {
     stopf("Task '%s' is '%s', but learner '%s' is for '%s'!", td$id, td$type, learner$id, learner$type)
   }
 

2.  measures.R

mmce = makeMeasure(id = "mmce", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi", "tsclassif", "req.pred", "req.truth"),
  name = "Mean misclassification error",
  note = "Defined as: mean(response != truth)",
  fun = function(task, model, pred, feats, extra.args) {
    measureMMCE(pred$data$truth, pred$data$response)
  }
)

tpr = makeMeasure(id = "tpr", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "tsclassif", "req.pred", "req.truth"),
  name = "True positive rate",
  note = "Percentage of correctly classified observations in the positive class. Also called hit rate or recall.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTPR(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

3. --- a/R/Task_operators.R
+++ b/R/Task_operators.R
@@ -439,7 +439,6 @@ changeData = function(task, data, costs, weights) {
     "classif" = makeTaskDesc(task, td$id, td$target, td$positive),
     "surv" = makeTaskDesc(task, td$id, td$target, td$censoring),
     "cluster" = makeTaskDesc(task, td$id),
-    "tsclassif" = makeTaskDesc(task, td$id, td$target, td$positive),
     makeTaskDesc(task, td$id, td$target))
   return(task)
 }

