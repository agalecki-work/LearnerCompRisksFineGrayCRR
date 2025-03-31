# Example: Variations of cov2_info in LearnerCompRisksFineGrayCRR
library(LearnerCompRisksFineGrayCRR)
library(mlr3)
library(mlr3proba)

# Load and prepare the pbc task
task <- tsk("pbc")
task$select(c("age", "bili", "sex"))
print(task)

# Variation 1: No cov2_info (fixed covariates only)
cat("\nVariation 1: No cov2_info (fixed covariates only)\n")
learner1 <- lrn("cmprisk.crr")
learner1$train(task)
pred1 <- learner1$predict(task)
cat("Model summary for event 1:\n")
print(learner1$model[[1]])
cat("Prediction object:\n")
print(pred1)

# Variation 2: Numeric covariates (age, bili) with log and quadratic transformations
cat("\nVariation 2: Numeric covariates (age, bili) with log and quadratic tf\n")
learner2 <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), uft^2)
  )
)
learner2$train(task)
pred2 <- learner2$predict(task)
cat("Model summary for event 1:\n")
print(learner2$model[[1]])
cat("Prediction object:\n")
print(pred2)

# Variation 3: Mixed numeric and factor (age, sex) with log transformations
cat("\nVariation 3: Mixed numeric and factor (age, sex) with log transformations\n")
learner3 <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "sex"),
    tf = function(uft) cbind(log(uft), log(uft + 1))
  )
)
learner3$train(task)
pred3 <- learner3$predict(task)
cat("Model summary for event 1:\n")
print(learner3$model[[1]])
cat("Prediction object:\n")
print(pred3)

# Variation 4: Repeated covariate (age) with linear and exponential transformations
cat("\nVariation 4: Repeated covariate (age) with linear and exponential tf\n")
learner4 <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "age"),
    tf = function(uft) cbind(uft, exp(uft / 1000))
  )
)
learner4$train(task)
pred4 <- learner4$predict(task)
cat("Model summary for event 1:\n")
print(learner4$model[[1]])
cat("Prediction object:\n")
print(pred4)

# Variation 5: Single covariate (bili) with cubic transformation
cat("\nVariation 5: Single covariate (bili) with cubic tf\n")
learner5 <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("bili"),
    tf = function(uft) matrix(uft^3, ncol = 1)
  )
)
learner5$train(task)
pred5 <- learner5$predict(task)
cat("Model summary for event 1:\n")
print(learner5$model[[1]])
cat("Prediction object:\n")
print(pred5)

# Variation 6: cov2only with bili as time-varying only
cat("\nVariation 6: cov2only with bili as time-varying only\n")
learner6 <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), uft),
    cov2only = c("bili")
  )
)
learner6$train(task)
pred6 <- learner6$predict(task)
cat("Model summary for event 1:\n")
print(learner6$model[[1]])
cat("Prediction object:\n")
print(pred6)