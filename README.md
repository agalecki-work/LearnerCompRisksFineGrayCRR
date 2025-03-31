# LearnerCompRisksFineGrayCRR

A Fine-Gray Competing Risks Regression learner for `mlr3proba`, built on `cmprsk::crr()`. This package allows modeling of cumulative incidence functions (CIFs) for competing risks, supporting both fixed and time-varying covariates.

## Installation

Install the package directly from GitHub:

```
library(devtools)
install_github("agalecki-work/LearnerCompRisksFineGrayCRR")Verify:

```

## Verification

```
library(mlr3)
library(LearnerCompRisksFineGrayCRR)
learner <- lrn("cmprsk.crr")
print(learner$id)  # Should print "cmprsk.crr"
```

## Examples

Explore example usage included with the package:


```
# Locate the examples directory
pkg = "LearnerCompRisksFineGrayCRR"
system.file("examples", package= pkg)
source(system.file("examples/example-cov2-variations.R", package = pkg))

```

For detailed documentation, see `?LearnerCompRisksFineGrayCRR` after loading the package.

