#' @title Set missing factor levels missing in train data to NA
#' @description
#' Accounts for missing factor levels only present in test data
#' but not in train data by setting levels to NA
#' @param model \cr
#'   fitted model on training data
#' @param test.data [`data.frame`] \cr
#'   data to make predictions for
#' @return `data.frame` with matching factor levels to fitted model
missingLevelsTrain = function(model, test.data) {

  # https://stackoverflow.com/a/39495480/4185785

  # drop empty factor levels in test data
  test.data = as.data.frame(droplevels(test.data))

  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors = stri_replace_all_regex(names(unlist(fit$learner.model$contrasts)),
                                     "[-^0-9]|as.factor|\\(|\\)", "")
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test.data)
    }

    factor.levels = unlist(lapply(fit$contrasts, function(x)
      outer(rownames(x), colnames(x), paste, sep = ":")))
    factor.levels = stri_split(factor.levels, ":", simplify = TRUE)[, 1]

    model.factors = as.data.frame(cbind(factors, factor.levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors = stri_replace_all_regex(names(unlist(fit$learner.model$xlevels)),
                                     "[-^0-9]|as.factor|\\(|\\)", "")

    if (length(factors) == 0) {
      return(test.data)
    }
    factor.levels = unname(unlist(fit$learner.model$xlevels))
    model.factors = as.data.frame(cbind(factors, factor.levels))
  }

  # Select column names in test data that are factor predictors in
  # trained model
  predictors = names(test.data[names(test.data) %in% factors])

  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA

  for (i in seq_len(length(predictors))) {
    found = test.data[, predictors[i]] %in% model.factors[
      model.factors$factors == predictors[i], ]$factor.levels
    if (any(!found)) {
      # track which variable
      var = predictors[i]
      test.data[!found, predictors[i]] = NA
      test.data = droplevels(test.data)
      # issue warning to console
      catf("Setting missing levels in '%s', only present in test data but missing in train data, to 'NA'.",
                      var)
      catf("This affected %s of %s total observations in test data.",
           table(is.na(test.data))[[2]], nrow(test.data))
    }
  }
  return(test.data)
}
