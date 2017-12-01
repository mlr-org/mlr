# mlr 2.12

## Major
- Support for functional data (fda) using matrix columns has been added.
- Relaxed the way wrappers can be nested. the only explicitly forbidden combination is to wrap a tuning wrapper around another optimization wrapper
- Refactored the resample progress messages to give a better overview and distinguish between train and test measures better
- calculateROCMeasures now returns absolute instead of relative values

