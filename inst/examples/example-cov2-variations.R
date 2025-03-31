# Example usage of LearnerCompRisksFineGrayCRR with various cov2_info configurations

library(mlr3)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)

# Prepare the task
task <- tsk("pbc")
task$select(c("age", "sex", "bili"))
set.seed(123)

# 1. No cov2_info: All covariates treated as fixed
learner_no_cov2 <- lrn("cmprisk.crr")
learner_no_cov2$train(task)
pred_no_cov2 <- learner_no_cov2$predict(task)
print(pred_no_cov2)

# 2. Numeric predictors with a two-column transformation function
learner_numeric <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), log(uft + 1))
  )
)
learner_numeric$train(task)
pred_numeric <- learner_numeric$predict(task)
print(pred_numeric)

# 3. Mixed numeric and factor variables with a two-column transformation
learner_mixed <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "sex"),
    tf = function(uft) cbind(log(uft), log(uft + 1))
  )
)
learner_mixed$train(task)
pred_mixed <- learner_mixed$predict(task)
print(pred_mixed)

# 4. Repeated covariates in cov2nms
learner_repeats <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "age"),
    tf = function(uft) cbind(log(uft), uft)
  )
)
learner_repeats$train(task)
pred_repeats <- learner_repeats$predict(task)
print(pred_repeats)

# 5. cov2only: Bili as time-varying only
learner_cov2only <- lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), uft),
    cov2only = c("bili")
  )
)
learner_cov2only$train(task)
pred_cov2only <- learner_cov2only$predict(task)
print(pred_cov2only)
print(names(learner_cov2only$state$model[[1]]$coef))  # Check coefficients