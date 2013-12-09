Integrating another learner
===========================

In order to create a new learner in **mlr** an interface code has to be written. This can
easily be done by either inheriting from rlearner.classif or rlearner.regr. In the following, 
an example of how the Linear Discriminant Analysis from the package ``MASS`` has been 
integrated into the classification learner ``classif.lda`` in **mlr** is presented.

Initialization
--------------

For initialization, all required information you need is the name of the learner, its package 
of origin, the parameter set and the set of properties of your learner.

First, name your learner and replace it all necessary parts of the code. The naming conventions 
in **mlr** are ``classif.<R_method_name>`` for classification and ``regr.<R_method_name>`` for 
regression. Second of all, use [makeDiscreteLearnerParam](http://berndbischl.github.io/ParamHelpers/man/LearnerParam.html) and [makeNumericLearnerParam](http://berndbischl.github.io/ParamHelpers/man/LearnerParam.html) to incorporate 
the complete description of the parameters. Include all possible values for discrete parameters, aswell 
as lower and upper bounds for numeric parameters. Also, add information for the properties (see also 
the Section about [Learners](learner.md)). Which types of predictors that are supported (numerics, factors)? 
Are case weights supported? Can the method deal with missing values in the predictor variables and deal with 
NAs in a meaningful way (not na.omit)? Are oneclass, twoclass, multiclass problems supported? Can the 
learner predict posterior probabilities? In the regression case, you do not need to take care of the 
latter properties.

In the following example, you see the R-code for Linear Discriminant Analysis. The name is 
``"classif.lda"`` from the package ``MASS``. LDA has one discrete parameter, ``method``, and two 
continuous ones, ``nu`` and ``tol``. It supports classification problems with two or more classes and 
can deal with numeric and factor explanatory variables. It can predict posterior probabilities.


```splus
makeRLearner.classif.lda = function() {
    makeRLearnerClassif(cl = "classif.lda", package = "MASS", par.set = makeParamSet(makeDiscreteLearnerParam(id = "method", 
        default = "moment", values = c("moment", "mle", "mve", "t")), makeNumericLearnerParam(id = "nu", 
        lower = 2, requires = expression(method == "t")), makeNumericLearnerParam(id = "tol", 
        default = 1e-04, lower = 0)), twoclass = TRUE, multiclass = TRUE, numerics = TRUE, 
        factors = TRUE, prob = TRUE)
}
```


Creating the training function of the learner
---------------------------------------------

This must fit a model on the data of the task ``.task`` with regard to the subset defined in the 
integer vector ``.subset`` and the parameters passed in the ``...`` arguments. It must return the 
fitted model, no special data type is assumed for this. For further information, read the 
documentation of [getTaskData](http://berndbischl.github.io/mlr/man/getTaskData.html).

In the example, replace lda by the name of the training function of your method. Pass all 
required arguments to the training function. The data can be extracted from the task via the 
[getTaskData](http://berndbischl.github.io/mlr/man/getTaskData.html) function. Pass further arguments like case weights via ``.weights`` to the training method.


```splus
trainLearner.classif.lda = function(.learner, .task, .subset, .weights, ...) {
    f = getTaskFormula(.task)
    lda(f, data = getTaskData(.task, .subset), ...)
}
```


Creating the prediction method 
------------------------------

This must predict all the new observations in the data frame ``.newdata`` with the wrapped model ``.model.`` 
Simply access the real fitted model by ``.model$learner.model``. For regression: you have to return a 
numeric vector of predicted response values. For classification: you have to return a factor of predicted
classes if ``.learner$predict.type`` is ``"response"``, or you have return a matrix of predicted probabilities if
``.learner$predict.type`` is ``"prob"``. In the latter case the matrix must have the same number of columns as there
are classes in the task and the columns have to be named by the class names.


```splus
predictLearner.classif.lda = function(.learner, .model, .newdata, ...) {
    p = predict(.model$learner.model, newdata = .newdata, ...)
    if (.learner$predict.type == "response") 
        return(p$class) else return(p$posterior)
}
```




