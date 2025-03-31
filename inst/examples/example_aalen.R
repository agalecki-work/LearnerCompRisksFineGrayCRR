# cmprsk.aalen

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
learner = lrn("cmprsk.aalen")
learner$train(task, part$train)
names(learner)
learner$model
# pred = learner$train(task, part$train)$predict(task, part$test)