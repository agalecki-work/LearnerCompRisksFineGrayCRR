# cmprsk.aalen. Learner defined in `mlr3proba`. Included for comparison

library(mlr3)
library(mlr3proba)
library(data.table)

# Load and prepare the pbc task
task = tsk("pbc")
task$select(c("age", "bili", "sex"))
cat("\n -- pbc task")
print(task)

# Initial partition
set.seed(123)
part = partition(task, ratio=0.7)

learner = lrn("cmprsk.aalen")
learner$train(task, part$train)

cat("\n -- Model fit")
print(learner$model)

cat("\n -- Predicted values")
pred = learner$train(task, part$train)$predict(task, part$test)

cat("\n -- CIF structure")
str(pred$cif)
