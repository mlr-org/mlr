pkgs <- c(
  "devtools",
  "roxygen2",
  "testthat",

  "abind", 
  "BBmisc", 
  "codetools", 
  "ada", 
  "adabag", 
  "DiceKriging", 
  "e1071",
  "earth", 
  "FNN",
  "FSelector",
  "gbm", 
  "kernlab", 
  "kknn", 
  "klaR",
  "mboost", 
  "mda", 
  "nnet", 
  "ParamHelpers", 
  "party",
  "penalized", 
  "pls",
  "randomForest",
  "robustbase",
  "rpart", 
  "ROCR",
  "pROC",
  "rsm",
  "RWeka"
)

# Determine missing packages 
installed.pkgs <- rownames(installed.packages())
missing.pkgs <- setdiff(pkgs, installed.pkgs)

if(length(missing.pkgs) > 0) {
  cat("Installing the following packages:\n")
  print(missing.pkgs)
  install.packages(missing.pkgs, dep = TRUE, repos="http://cran.at.r-project.org")
} else {
  cat("All packages installed!\n")
}


