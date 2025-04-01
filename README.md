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
# list of examples in the examples directory 
xpath =system.file("examples", package= "LearnerCompRisksFineGrayCRR")
(exList= list.files(xpath, pattern = "*.R"))

```

```
# Select example from exList vector
ex = exList[1]
source(system.file(paste0("examples/", ex), package = "LearnerCompRisksFineGrayCRR"))

```

For detailed documentation, see `?LearnerCompRisksFineGrayCRR` after loading the package.

