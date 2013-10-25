mlr
===

  mlr: Machine Learning in R 

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
  
  Installation
==============
  
  1) Normal users:
  Please use the CRAN version linked above.

2) Early adopters: Simply running
```r
install_github("mlr", username="berndbischl")
```
will install the current github version.

3) Assuming you have a reasonably configured OS and R, you could also build and run tasks via the MAKEFILE.
But only a VERY SMALL percentage of users should be interested in this, instead of options 1) or 2).

- Clone from git repo here

- Have recent version of R properly installed with all build tools. For Windows this will include 

http://cran.r-project.org/bin/windows/Rtools/
  
  - Have git as a command-line tool available.

- Have roxygen2, devtools and testhat R packages installed

- In a console run "make install" to install. Done.

- "make" will list all other build targets

- If you have problems (e.g. in Windows) because there is no "git" command line
tool that we use to figure out the build number, remove these lines from the Makefile:
  
  ```
echo "Setting version ...
  ${RSCRIPT} ./tools/set-version
```
