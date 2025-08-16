# Load required libraries
library(mlr3)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)

# Define the learner
learner = lrn("cmprsk.crr")

# Define a task
task = tsk("pbc")
task$select(c("age", "bili", "sex"))
task$set_col_roles(cols = "status", add_to = "stratum")

# Create train and test sets
ids = partition(task)

# Train the learner
learner$train(task, row_ids = ids$train)

# Print model and importance
print(learner$model)
print(learner$importance(cause = "1")) # Importance for cause 1
print(learner$importance(cause = "2")) # Importance for cause 2

# Make predictions
predictions = learner$predict(task, row_ids = ids$test)
