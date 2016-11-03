#' Multivariate Adaptive Regression Splines
#'
#' Build a regression model using the techniques in Friedman's papers "Multivariate Adaptive Regression Splines"
#' and "Fast MARS".
#'
#' find more information about the alogorithm and its parameters here: http://www.milbo.org/doc/earth-notes.pdf
#' @param keepxy
#' Default is FALSE. Set to TRUE to retain the following in the returned value: x and y (or data), subset, and weights. The function update.earth and friends will use these if present instead of searching for them in the environment at the time update.earth is invoked.
#' When the nfold argument is used with keepxy=TRUE, earth keeps more data and calls predict.earth multiple times to generate cv.oof.rsq.tab and cv.infold.rsq.tab (see the cv. arguments in the "Value" section below). It therefore makes cross-validation significantly slower.
#' @param trace
#' Trace earth's execution. Default is 0. Values:
#' \describe{
#' \item{0}{no tracing}
#' \item{.3}{variance model (the varmod.method arg)}
#' \item{.5}{cross validation (the nfold arg)}
#' \item{1}{overview}
#' \item{2}{forward pass}
#' \item{3}{pruning}
#' \item{4}{model mats summary, pruning details}
#' \item{5}{full model mats, internal details of operation}
#' }
#' @param glm
#' NULL (default) or a list of arguments to pass on to glm. See the documentation of glm for a description of these arguments See "Generalized linear models" in the vignette.
#'  Example: earth(survived~., data=etitanic, degree=2, glm=list(family=binomial))
#'
#' The following arguments are for the forward pass.
#' @param degree
#' Maximum degree of interaction (Friedman's mi). Default is 1, meaning build an additive model (i.e., no interaction terms).
#' @param penalty
#' Generalized Cross Validation (GCV) penalty per knot. Default is if(degree>1) 3 else 2. Simulation studies suggest values in the range of about 2 to 4. The FAQ section in the vignette has some information on GCVs.
#' Special values (for use by knowledgeable users): The value 0 penalizes only terms, not knots. The value -1 means no penalty, so GCV = RSS/n.
#' @param nk
#' Maximum number of model terms before pruning, i.e., the maximum number of terms created by the forward pass. Includes the intercept.
#' The actual number of terms created by the forward pass will often be less than nk because of other stopping conditions. See "Termination conditions for the forward pass" in the vignette.
#' The default is semi-automatically calculated from the number of predictors but may need adjusting.
#' @param thresh
#' Forward stepping threshold. Default is 0.001. This is one of the arguments used to decide when forward stepping should terminate: the forward pass terminates if adding a term changes RSq by less than thresh. See "Termination conditions for the forward pass" in the vignette.
#' @param minspan
#' Minimum number of observations between knots. (This increases resistance to runs of correlated noise in the input data.)
#' The default minspan=0 is treated specially and means calculate the minspan internally, as per Friedman's MARS paper section 3.8 with alpha = 0.05. Set trace>=2 to see the calculated value.
#' Use minspan=1 and endspan=1 to consider all x values.
#' Negative values of minspan specify the maximum number of knots per predictor. These will be equally spaced. For example, minspan=-3 allows three evenly spaced knots for each predictor. As always, knots that fall in the endzones specified by endspan will be ignored.
#' @param endspan
#' Minimum number of observations before the first and after the final knot.
#' The default endspan=0 is treated specially and means calculate the minspan internally, as per the MARS paper equation 45 with alpha = 0.05. Set trace>=2 to see the calculated value.
#' Be wary of reducing endspan, especially if you plan to make predictions beyond or near the limits of the training data. Overfitting near the edges of training data is much more likely with a small endspan. The model's RSq and GRSq won't indicate when this overfitting is occurring. (A plotmo plot can help: look for sharp hinges at the edges of the data). See also the Adjust.endspan argumen.
#' @param newvar.penalty
#' Penalty for adding a new variable in the forward pass (Friedman's gamma, equation 74 in the MARS paper). Default is 0, meaning no penalty for adding a new variable. Useful non-zero values typically range from about 0.01 to 0.2 and sometimes higher - you will need to experiment.
#' A word of explanation. With the default newvar.penalty=0, if two variables have nearly the same effect (e.g. they are collinear), at any step in the forward pass earth will arbitrarily select one or the other (depending on noise in the sample). Both variables can appear in the final model, complicating model interpretation. On the other hand with a non-zero newvar.penalty, the forward pass will be reluctant to add a new variable - it will rather try to use a variable already in the model, if that does not affect RSq too much. The resulting final model may be easier to interpret, if you are lucky. There will often be a small performance hit (a worse GCV).
#' @param fast.k
#' Maximum number of parent terms considered at each step of the forward pass. (This speeds up the forward pass. See the Fast MARS paper section 3.0.)
#' Default is 20. A value of 0 is treated specially (as being equivalent to infinity), meaning no Fast MARS. Typical values, apart from 0, are 20, 10, or 5.
#' In general, with a lower fast.k (say 5), earth is faster; with a higher fast.k, or with fast.k disabled (set to 0), earth builds a better model. However, because of random variation this general rule often doesn't apply.
#' @param fast.beta
#' Fast MARS ageing coefficient, as described in the Fast MARS paper section 3.1. Default is 1. A value of 0 sometimes gives better results.
#' @param linpreds
#' Index vector specifying which predictors should enter linearly, as in lm. The default is FALSE, meaning all predictors enter in the standard MARS fashion, i.e., in hinge functions.
#' This does not say that a predictor must enter the model; only that if it enters, it enters linearly. See "The linpreds argument" in the vignette.
#' A predictor's index in linpreds is the column number in the input matrix x (after factors have been expanded).
#' linpreds=TRUE makes all predictors enter linearly (the TRUE gets recycled).
#' linpreds may also be a character vector e.g. linpreds=c("wind", "vis"). Note: grep is used for matching. Thus "wind" will match all variables that have "wind" in their names. Use "^wind$" to match only the variable named "wind".
 #' @param allowed
#' Function specifying which predictors can interact and how. Default is NULL, meaning all standard MARS terms are allowed.
#' During the forward pass, earth calls the allowed function before considering a term for inclusion; the term can go into the model only if the allowed function returns TRUE. See "The allowed argument" in the vignette.
#'
#' The following arguments are for the pruning pass.
#' @param pmethod
#' Pruning method. One of: backward none exhaustive forward seqrep cv.
#' Default is "backward".
#' New in version 4.4.0: Specify pmethod="cv" to use cross-validation to select the number of terms. This selects the number of terms that gives the maximum mean out-of-fold RSq on the fold models. Requires the nfold argument.
#' Use "none" to retain all the terms created by the forward pass.
#' If y has multiple columns, then only "backward" or "none" is allowed.
#' Pruning can take a while if "exhaustive" is chosen and the model is big (more than about 30 terms). The current version of the leaps package used during pruning does not allow user interrupts (i.e., you have to kill your R session to interrupt; in Windows use the Task Manager or from the command line use taskkill).
#' @param nprune
#' Maximum number of terms (including intercept) in the pruned model. Default is NULL, meaning all terms created by the forward pass (but typically not all terms will remain after pruning). Use this to enforce an upper bound on the model size (that is less than nk), or to reduce exhaustive search time with pmethod="exhaustive".
#'
#' The following arguments are for cross validation.
#' @param ncross
#' Only applies if nfold>1. Number of cross-validations. Each cross-validation has nfold folds. Default 1.
#' @param nfold
#' Number of cross-validation folds. Default is 0, no cross validation. If greater than 1, earth first builds a standard model as usual with all the data. It then builds nfold cross-validated models, measuring R-Squared on the out-of-fold (left out) data each time. The final cross validation R-Squared (CVRSq) is the mean of these out-of-fold R-Squareds.
#' The above process of building nfold models is repeated ncross times (by default, once). Use trace=.5 to trace cross-validation.
#' Further statistics are calculated if keepxy=TRUE or if a binomial or poisson model (specified with the glm argument). See "Cross validation" in the vignette.
#' @param stratify
#' Only applies if nfold>1. Default is TRUE. Stratify the cross-validation samples so that an approximately equal number of cases with a non-zero response occur in each cross validation subset. So if the response y is logical, the TRUEs will be spread evenly across folds. And if the response is a multilevel factor, there will be an approximately equal number of each factor level in each fold (because a multilevel factor response gets expanded to columns of zeros and ones, see "Factors" in the vignette). We say "approximately equal" because the number of occurrences of a factor level may not be exactly divisible by the number of folds.

#' @export
makeRLearner.classif.earth = function() {
  makeRLearnerClassif(
    cl = "classif.earth",
    package = "earth",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "trace", default = 0, upper = 10, tunable = FALSE),
      makeLogicalLearnerParam(id = "keepxy",default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "degree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "penalty"),
      makeIntegerLearnerParam(id = "nk", lower = 0L),
      makeNumericLearnerParam(id = "thres", default = 0.001),
      makeIntegerLearnerParam(id = "minspan", default = 0L),
      makeIntegerLearnerParam(id = "endspan", default = 0L),
      makeNumericLearnerParam(id = "newvar.penalty", default = 0),
      makeIntegerLearnerParam(id = "fast.k", default = 20L, lower = 0L),
      makeNumericLearnerParam(id = "fast.beta", default = 1),
      makeDiscreteLearnerParam(id = "pmethod", default = "cv",
                               values = c("backward", "none", "exhaustive", "forward", "seqrep", "cv")),
      makeIntegerLearnerParam(id = "nprune"),
      makeIntegerLearnerParam(id = "ncross", default = 1L),
      makeIntegerLearnerParam(id = "nfold", default = 1L),
      makeLogicalLearnerParam(id = "stratify",default = TRUE),
      makeUntypedLearnerParam(id = "linpreds",default = FALSE),
      makeDiscreteLearnerParam("link", values = c("logit", "probit"),
                               default = "logit"),
      makeNumericLearnerParam(id = "maxit", default = 25L, tunable = FALSE),
      makeFunctionLearnerParam(id = "allowed")
    ),
    properties = c("twoclass", "numerics", "factors", "prob","weights"),
    name = "Flexible Discriminant Analysis",
    short.name = "fda",
    note = "This learner performs flexible discriminant analsis using the earth algorithm."
  )
}

#' @export
trainLearner.classif.earth = function(.learner, .task, .subset, .weights = NULL, link = "logit", maxit = 25L, ...) {
  f = getTaskFormula(.task)
  earth::earth(f, data = getTaskData(.task, .subset), glm = list(family = binomial(link = link) , maxit = maxit), ...)
}

#' @export
predictLearner.classif.earth = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$factor.levels[[1]]
  if (.learner$predict.type == "prob") {
    p = setColNames(cbind(1 - p, p), levs)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
  return(p)
}

