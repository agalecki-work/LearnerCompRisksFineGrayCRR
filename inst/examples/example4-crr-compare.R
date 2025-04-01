# Example usage of LearnerCompRisksFineGrayCRR compared to crr
library(mlr3)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)
library(data.table)

cat("\n -- `example4-crr-compare.R` executed \n")
# Prepare the task
task <- tsk("pbc")
task$select(c("sex", "bili", "age"))
cat("\n task with selected predictors \n")
print(task)


# 3. Mixed factor variables with a two-column transformation

tf = function(uft){
    tmtx =cbind(uft, uft)
    colnames(tmtx) = c("tm_lin", "tm_lin")
    tmtx
    }
    

learner_mixed <- lrn("cmprsk.crr",
  cov2_info = list(
    cov2nms = c("sex","bili"),
    tf = tf
  )
)
learner_mixed$train(task)
cat("\n -- FG Model (mlr3):  Mixed numeric and factor variables with a two-column transformation \n")
print(learner_mixed$model)

# For comparison use `cmprsk.crr` directly

library(cmprsk)
dt = as.data.table(task)
time = dt$time
status = dt$status
cov1 = model.matrix(~ age + bili + sex, data = dt)[,-1]
cov2 = model.matrix(~ sex + bili, data = dt)[, -1]

cat("\n -- crr results for failcode = 1 \n")
print(crr(time, status, cov1, cov2,tf, failcode=1))

cat("\n -- crr results for failcode = 2 \n")
crr(time, status, cov1, cov2,tf, failcode=2)