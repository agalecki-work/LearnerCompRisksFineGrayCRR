# Example with LearnerCompRisksFineGrayCRR
library(LearnerCompRisksFineGrayCRR)
library(mlr3)
library(mlr3proba)
library(data.table)

cat("\n -- example2-cov1only.R executed")
# Load and prepare the pbc task
task = tsk("pbc")
task$select(c("age", "bili", "sex"))
cat("\n -- pbc task \n")
print(task)

# Initial partition
set.seed(123)
part = partition(task, ratio=0.7)

# Define learner
 
crr_learner = lrn("cmprsk.crr")
crr_learner$train(task, part$train)
cat("\n -- FG model fit \n")
print(crr_learner$model)

cat("\n -- predicted values \n")
pred = crr_learner$train(task, part$train)$predict(task, part$test)
print(pred)
cat("\n -- CIF list structure \n") 
print(str(pred$cif))
