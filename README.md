![mlr](https://raw.githubusercontent.com/berndbischl/mlr/gh-pages/images/mlrLogo_blue_141x64.png): Machine Learning in R
==========================
[![Build Status](https://travis-ci.org/berndbischl/mlr.png)](https://travis-ci.org/berndbischl/mlr)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/mlr)](http://cran.r-project.org/web/packages/mlr)

* [Offical CRAN release site](http://cran.r-project.org/web/packages/mlr/)
* [Detailed Tutorial](http://berndbischl.github.io/mlr/tutorial/html/) ([online](http://berndbischl.github.io/mlr/tutorial/html/), [download for offline usage](http://berndbischl.github.io/mlr/tutorial/mlr_tutorial.zip))
* [R Documentation in HTML](http://www.rdocumentation.org/packages/mlr)
* Install the development version

    ```splus
    devtools::install_github("berndbischl/mlr")
    ```
* [Further installation instructions](https://github.com/tudo-r/PackagesInfo/wiki/Installation-Information)


Introduction
============

R does not define a standardized interface for all its machine learning algorithms. Therefore, for any
non-trivial experiments, you need to write lengthy, tedious and error-prone wrappers to call the different
algorithms and unify their respective output. Additionally you need to implement infrastructure to resample
your models, optimize hyperparameters, select features, cope with pre- and post-processing of data and
compare models in a statistically meaningful way.
As this becomes computationally expensive, you might want to parallelize your experiments as well. This
often forces users to make crummy trade-offs in their experiments due to time constraints or lacking expert
programming skills. **mlr** provides this infrastructure so that you can focus on your experiments!
The framework currently focuses on supervised methods like classification, regression and survival analysis and
their corresponding evaluation and optimization. It is written in a way that you can extend it yourself or
deviate from the implemented convenience methods and construct your own complex experiments or algorithms.

Features
========

* Clear S3 interface to R classification, regression, clustering and survival analysis methods
* Possibility to fit, predict, evaluate and resample models
* Easy extension mechanism through S3 inheritance
* Abstract description of learners and tasks by properties
* Parameter system for learners to encode data types and constraints
* Many convenience methods and generic building blocks for your
  machine learning experiments
* Resampling like bootstrapping, cross-validation and subsampling
* Different visualizations for e.g. ROC curves and predictions
* Benchmarking of learners for muliple data sets
* Easy hyperparameter tuning using different optimization strategies, including potent configurators
  like iterated F-racing (irace) or sequential model-based optimization
* Variable selection with filters and wrappers
* Nested resampling of models with tuning and feature selection
* Cost-sensitive learning, threshold tuning and imbalance correction
* Wrapper mechanism to extend learner functionality and complex and custom ways
* Combine different processing steps to a complex data mining chain that can be jointly optimized
* OpenML connector for the Open Machine Learning server
* Extension points to integrate your own stuff
* Parallelization is built-in
* Unit-testing

*If you like the package, please "star" it on Github.*


News
====
Most NEWS regarding extensions and changes of the packages can be accessed here for the
[release](http://cran.r-project.org/web/packages/mlr/NEWS) and here for the
[devel](https://github.com/berndbischl/mlr/blob/master/NEWS) version on Github.

* 2015-04-30:
  * I (Bernd) was pretty busy as I had to change cities and workplaces. I now head the Computational Statistics Group at LMU Munich. More importantly, this resulted in me not taking care of requests and issues as much as I wanted during the last weeks. Apologies and hopefully I have more time from now on.
  * mlr got not one, but three project slots in Google Summer of Code 2015. Many thanks to the R Foundation, Google and all students who applied with exciting proposals. Best of luck to Tong, Zach and Pascal, who will work on SVM ensembles, mlr's visualization system and better hyperparameter / tuning options.
* 2015-02-17:
  * **We have been informed that our tutorial "Applied Machine Learning and Efficient Model Selection with mlr" has been accepted for [useR 2015](http://user2015.math.aau.dk/) in Aarlborg. Hoping to meet all of you there in June!**
* 2015-02-04:
  * mlr 2.3 released to CRAN.
* 2014-10-28:
  * mlr 2.2 released to CRAN.
  * The popular Java tool WEKA uses mlr in its [RPlugin](http://weka.sourceforge.net/packageMetaData/RPlugin/index.html).
* 2014-10-18:
  * We have improved the tutorial A LOT. It is also based on mkdocs and knitr now. Still fixing minor things and extending a bit. Take a look yourself.
  * We might refactor the internal handling and OO structure of tasks a bit, so even more natural indexing and operations are possible. Michel is currently working on this in a branch.
  * I will talk about mlr in connection with the [OpenML](http://www.openml.org) project next week. If you don't know this, take a look, a very cool initiative by Joaquin Vanschoren. We are developing a connector to R in general which also covers mlr [here](https://github.com/openml/r).


Talks and Videos
================
* [Video](http://www.youtube.com/watch?v=rzjkT1uLNi4) of Bernd's "mlr + OpenML" talk at OpenML workshop 2014


Get in Touch
============

Please use the issue tracker for problems, questions and feature requests.
Don't email in most cases, as we forget these mails.

We also do not hate beginners and it is perfectly valid to mark a issue as "Question".

Please don't forget that all of us work in academia and put a lot of work into this project, simply because we like it, not because we are specifically paid for it.

We also welcome pull requests or new developers.
To get started have a look at the [guidelines for contributors](https://github.com/berndbischl/mlr/wiki/Setup-and-Contribution-Guidelines).
Please also consider the [**mlr** coding guidelines](https://github.com/berndbischl/mlr/wiki/mlr-Coding-Guidelines).

For everything else the maintainer Bernd Bischl can be reached here: bernd_bischl@gmx.net.
He (=me) is sometimes busy, so please use the other channels for appropriate stuff first, so you get quicker responses ;-)


