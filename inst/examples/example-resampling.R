
# Example: Resampling using cross-validation with LearnerCompRisksFineGrayCRR
library(LearnerCompRisksFineGrayCRR)
library(mlr3)
library(mlr3proba)
# library(mlr3tuning)

# Load and prepare the pbc task
task <- tsk("pbc")
task$select(c("age", "bili", "sex"))

# Define learner with time-varying covariates
learner <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "sex"),
    tf = function(uft) cbind(log(uft), log(uft + 1))
  )
)

# Define resampling strategy (3-fold cross-validation)
resampling <- rsmp("cv", folds = 3)

# Perform resampling
rr <- resample(task, learner, resampling)

# View predictions for one fold
print(rr$prediction(1))
