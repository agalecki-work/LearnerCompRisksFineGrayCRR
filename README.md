# LearnerCompRisksFineGrayCRR

An `mlr3` learner for Fine-Gray competing risks regression using `cmprsk::crr()`.

## Installation

Install the package directly from GitHub:

```
devtools::install_github("agalecki-work/LearnerCompRisksFineGrayCRR",
                         build_vignettes= TRUE)
```

## Requirement

Check whether `LearnerCompRisks` generator is defined in your `mlr3proba` installation

```
class(mlr3proba::LearnerCompRisks) # Should print "R6ClassGenerator"

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
# list examples in the examples directory 
xpath = system.file("examples", package= "LearnerCompRisksFineGrayCRR")
(exList = list.files(xpath, pattern = "*.R"))

```

```
# Select example from `exList` vector
ex = exList[1]
source(system.file(paste0("examples/", ex), package = "LearnerCompRisksFineGrayCRR"))

```

For detailed documentation, see `?LearnerCompRisksFineGrayCRR` after loading the package.

