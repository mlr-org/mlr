mlr: Machine Learning in R 
==========================

  Introduction
==============

  R does not define a standardized interface for all its machine learning algorithms. Therefore, for any 
  non-trivial experiments, you need to write lengthy, tedious and error-prone wrappers to call the different 
  algorithms and unify their respective output. Additionally you need to implement infrastructure to resample 
  your models, optimize hyperparameters, select features, cope with pre- and post-processing of data and 
  compare models in a statistically meaningful way.
  As this becomes computationally expensive, you might want to parallelize your experiments as well. This 
  often forces useRs to make crummy trade-offs in their experiments due to time constraints or lacking expert 
  programming skills. **mlr** provides this infrastructure so that you can focus on your experiments!
  The framework currently focuses on supervised methods like classification and regression and their 
  corresponding evaluation and optimization, but further extensions are planned. It is written in a way 
  that you can extend it yourself or deviate from the implemented convenience methods and construct your own 
  complex experiments or algorithms.

Offical CRAN release site: 
  http://cran.r-project.org/web/packages/mlr/index.html

R Documentation in HTML:
  http://www.statistik.tu-dortmund.de/~bischl/rdocs/mlr/
  
Detailed Tutorial:
  https://www.statistik.tu-dortmund.de/~bischl/mlr_tutorial/tutorial/index.html
  
  Features
==========

* Clear S3 interface to R classification and regression methods
* Easy extension mechanism through S3 inheritance
* Abstract description of learners and tasks by properties
* Parameter system for learners to encode data types and constraints
* Many convenience methods and generic building blocks for your
  machine learning experiments
* Resampling like bootstrapping, cross-validation and subsampling
* Easy hyperparameter tuning using different optimization strategies
* Variable selection with filters and wrappers
* Parallelization is built-in
* Extension points to integrate your own stuff
* Unit-testing
* Possibility to fit, predict, evaluate and resample models
* Tune hyper-parameters of a learner with different optimization algorithms (also available for multi-criteria optimization)
* Feature selection with filters and wrappers (also available for multi-criteria optimization)
* Combine different processing steps to a complex data mining chain; enables nested resampling of optimized models
* Optimize functions with a model-based sequential parameter optimization method (useful e.g. for the optimization of expensive black-box functions)

  
Installation
============

1) Normal users:
Please use the CRAN version linked above.

2) Early adopters: Simply running
```r
devtools::install_github("mlr", username="berndbischl")
```
will install the current github version.

3) Developers and hackers:

You can install a new package version after local code changes if you are in the checkout directory via
```r
devtools::install(".")
```
Assuming you have a reasonably configured OS and R, you could also build and run tasks via the MAKEFILE.
But only a VERY SMALL percentage of users should be interested in this.

- Clone from git repo here

- Have recent version of R properly installed with all build tools. For Windows this will include 
  
  http://cran.r-project.org/bin/windows/Rtools/

- Have R, Rscript and the binaries of Rtools in your PATH 

- Have roxygen2, devtools and testhat R packages installed

- In a console run "make install" to install. Done.

- "make" will list all other build targets


Mailinglist and Email-Service-Hook
==================================

Developers use this mailing list:

https://groups.google.com/forum/?hl=de#!forum/mlr-devel

mlr-devel@googlegroups.com

The github-service-hook will also send commit messages to this list. 


Get in touch
============

If you are interested in the package, have a question regarding the usage or a feature request,
or maybe want to help improving **mlr**, please send a mail to the list at
mlr-general@lists.r-forge.r-project.org or to the maintainer Bernd Bischl
at bischl@statistik.uni-dortmund.de.
