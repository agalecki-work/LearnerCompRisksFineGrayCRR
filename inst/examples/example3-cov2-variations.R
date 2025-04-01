# Example usage of LearnerCompRisksFineGrayCRR with various cov2_info configurations

library(mlr3)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)

# Prepare the task
task <- tsk("pbc")
task$select(c("age", "sex", "bili"))
cat("\n task with selected predictors")
print(task)

# 1. No cov2_info: All predictors treated as fixed
learner_no_cov2 <- lrn("cmprsk.crr")
learner_no_cov2$train(task)
cat("\n -- FG Model:  All predictors treated as fixed")
print(learner_no_cov2$model)

pred_no_cov2 <- learner_no_cov2$predict(task)
cat("\n Predicted values")
print(pred_no_cov2)

# 2. Numeric predictors with a two-column transformation function
learner_numeric <- lrn("cmprsk.crr",
  cov2_info = list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), log(uft + 1))
  )
)
learner_numeric$train(task)
cat("\n -- FG Model: Numeric predictors with a two-column transformation function")
print(learner_numeric$model)


# 3. Mixed numeric and factor variables with a two-column transformation
learner_mixed <- lrn("cmprsk.crr",
  cov2_info = list(
    cov2nms = c("age", "sex"),
    tf = function(uft) cbind(log(uft), log(uft + 1))
  )
)
learner_mixed$train(task)
cat("\n -- FG Model:  Mixed numeric and factor variables with a two-column transformation")
print(learner_mixed$model)


# 4. Repeated covariates in cov2nms
learner_repeats <- lrn("cmprsk.crr",
  cov2_info = list(
    cov2nms = c("age", "age"),
    tf = function(uft) cbind(log(uft), uft)
  )
)
learner_repeats$train(task)
cat("\n -- FG Model:  Repeated covariates in cov2nms")
print(learner_repeats$model)

# 5. cov2only: Bili as time-varying only
learner_cov2only <- lrn("cmprsk.crr",
  cov2_info = list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), uft),
    cov2only = c("bili")
  )
)
learner_cov2only$train(task)
learner_cov2only$predict(task)
cat("\n -- FG Model:  cov2only: Bili as time-varying only")
print(learner_cov2only$model)
