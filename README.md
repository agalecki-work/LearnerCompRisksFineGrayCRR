# LearnerCompRisksFineGrayCRR

```
library(devtools)
install_github("agalecki-work/LearnerCompRisksFineGrayCRR")
```
Verify:

```
library(mlr3)
library(LearnerCompRisksFineGrayCRR)
learner <- lrn("cmprsk.crr")
print(learner$id)  # Should print "cmprsk.crr"
```

Examples

```
pkg = "LearnerCompRisksFineGrayCRR"
system.file("examples", package= pkg)
source(system.file("examples/example1.R", package = pkg))

```