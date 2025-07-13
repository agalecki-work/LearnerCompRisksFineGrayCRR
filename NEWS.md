# LearnerCompRisksFineGrayCRR 0.3.1

- Added `.Rbuildignore` to exclude `.lintr`, `man-roxygen`, and `LICENSE.md` from package builds.
- Introduced `.lintr` configuration to enforce code style.
- Added helper functions in `tests/testthat/helper.R` for improved test modularity.
- Updated test suite in `test_cmprsk_crr.R` to use `source("helper.R")` for task configuration and partitioning.
- Added Roxygen2 documentation for R6 methods (`initialize`, `convergence`, `importance`) in `learner_cmprsk_crr.R`.
- Initial implementation of Fine-Gray competing risks regression learner for `mlr3proba`.

# LearnerCompRisksFineGrayCRR 0.1.2

- Initial release with basic Fine-Gray competing risks regression learner functionality.