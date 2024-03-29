% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFeatureImportance.R
\name{getFeatureImportance}
\alias{getFeatureImportance}
\title{Calculates feature importance values for trained models.}
\usage{
getFeatureImportance(object, ...)
}
\arguments{
\item{object}{(\link{WrappedModel})\cr
Wrapped model, result of \code{\link[=train]{train()}}.}

\item{...}{(any)\cr
Additional parameters, which are passed to the underlying importance value
generating function.}
}
\value{
(\code{FeatureImportance}) An object containing a \code{data.frame} of the
variable importances and further information.
}
\description{
For some learners it is possible to calculate a feature importance measure.
\code{getFeatureImportance} extracts those values from trained models.
See below for a list of supported learners.
}
\details{
\itemize{
\item boosting \cr
Measure which accounts the gain of Gini index given by a feature
in a tree and the weight of that tree.
\item cforest \cr
Permutation principle of the 'mean decrease in accuracy' principle in
randomForest. If \code{auc=TRUE} (only for binary classification), area under
the curve is used as measure.  The algorithm used for the survival learner
is 'extremely slow and experimental; use at your own risk'. See
\code{\link[party:varimp]{party::varimp()}} for details and further parameters.
\item gbm \cr
Estimation of relative influence for each feature. See
\code{\link[gbm:relative.influence]{gbm::relative.influence()}}
for details and further parameters.
\item h2o \cr
Relative feature importances as returned by
\code{\link[h2o:h2o.varimp]{h2o::h2o.varimp()}}.
\item randomForest \cr
For \code{type = 2} (the default) the 'MeanDecreaseGini' is measured, which is
based on the Gini impurity index used for the calculation of the nodes.
Alternatively, you can set \code{type} to 1, then the measure is the mean
decrease in accuracy calculated on OOB data. Note, that in this case the
learner's parameter \code{importance} needs to be set to be able to compute
feature importance values.
See \code{\link[randomForest:importance]{randomForest::importance()}} for details.
\item RRF \cr
This is identical to randomForest.
\item ranger \cr
Supports both measures mentioned above for the randomForest
learner. Note, that you need to specifically set the learners parameter
\code{importance}, to be able to compute feature importance measures.
See \code{\link[ranger:importance.ranger]{ranger::importance()}} and
\code{\link[ranger:ranger]{ranger::ranger()}} for details.
\item rpart \cr
Sum of decrease in impurity for each of the surrogate variables at each
node
\item xgboost \cr
The value implies the relative contribution of the corresponding feature
to the model calculated by taking each feature's contribution for each
tree in the model. The exact computation of the importance in xgboost is
undocumented.
}
}
