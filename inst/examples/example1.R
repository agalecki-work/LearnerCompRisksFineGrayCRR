# Example with LearnerCompRisksFineGrayCRR
library(LearnerCompRisksFineGrayCRR)
library(mlr3)
library(mlr3proba)
library(data.table)

# Load and prepare the pbc task
task = tsk("pbc")
task$select(c("age", "bili", "sex"))

# Initial partition
set.seed(123)
part = partition(task, ratio=0.7)

# Define learners with time-varying covariates

# 
crr_learner = lrn("cmprisk.crr",
  cov2_info = list(
    cov2nms = c("age", "sex"),
    tf = function(uft) cbind(log(uft), log(uft + 1))
  )
)
crr_learner$train(task, part$train)
names(crr_learner)

pred = crr_learner$train(task, part$train)$predict(task, part$test)
typeof(pred)
class(pred)
methods(class="Prediction")
methods(class="PredictionCompRisks")

pred_dt = as.data.table(pred)
pred_dt = dim(pred_dt)
dim(pred_dt) # 83 obs  x 4 vars
head(pred_dt)

str(pred$cif)
