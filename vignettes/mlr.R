## ------------------------------------------------------------------------
library(mlr)
data(iris)

## Define the task:
task = makeClassifTask(id = "tutorial", data = iris, target = "Species")
print(task)

## Define the learner:
lrn = makeLearner("classif.lda")
print(lrn)

## Define the resampling strategy:
rdesc = makeResampleDesc(method = "CV", stratify = TRUE)

## Do the resampling:
r = resample(learner = lrn, task = task, resampling = rdesc)
print(r)

## Get the mean misclassification error:
r$aggr

## ----echo=FALSE,results="asis"-------------------------------------------
parsePkgs = function(x) {
  x = strsplit(x, "\n|,")[[1L]]
  # remove version requirement in (...)
  x = sub("\\(.*\\)", "", x)
  # trim whitespace (cannot be inside name)
  x = gsub(" ", "", x)
  # empty string become char(0)
  x[nzchar(x)]
}

desc = packageDescription("mlr")
pkgs = c(parsePkgs(desc$Depends), parsePkgs(desc$Imports), parsePkgs(desc$Suggests))
pkgs = sort(setdiff(pkgs, c("R", "stats", "methods")))
cat(sprintf("* [%1$s](http://cran.r-project.org/package=%1$s)", pkgs), sep = "\n")

