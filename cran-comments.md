## Test environments

* ubuntu 16.04 (on travis-ci), R 3.5.3
* win-builder: R-devel

## R CMD check results

### Ubuntu

0 errors | 0 warnings | 1 notes

### Winbuilder

0 errors | 1 warnings | 1 notes

We cannot replicate the following errors on our Travis CI setup or locally.
All `.Rd` files exists and are created via `#' alias <name>`.

```sh
Missing link or links in documentation object 'ClassifTask.Rd':
  'CostSensTask' 'ClusterTask' 'MultilabelTask' 'SurvTask'

Missing link or links in documentation object 'CostSensTask.Rd':
  'ClassifTask' 'ClusterTask' 'MultilabelTask' 'SurvTask'

Missing link or links in documentation object 'MultilabelTask.Rd':
  'ClassifTask' 'CostSensTask' 'ClusterTask' 'SurvTask'

Missing link or links in documentation object 'RegrTask.Rd':
  'ClassifTask' 'CostSensTask' 'ClusterTask' 'MultilabelTask'
  'SurvTask'

Missing link or links in documentation object 'SurvTask.Rd':
  'ClassifTask' 'CostSensTask' 'ClusterTask' 'MultilabelTask'

Missing link or links in documentation object 'Task.Rd':
  'ClassifTask' 'CostSensTask' 'ClusterTask' 'MultilabelTask'
  'SurvTask'

Missing link or links in documentation object 'getTaskCosts.Rd':
  'CostSensTask'

Missing link or links in documentation object 'makeClusterTask.Rd':
  'ClassifTask' 'CostSensTask' 'MultilabelTask' 'SurvTask'
```

## revdepcheck results

We checked 20 reverse dependencies (19 from CRAN + 1 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
