# cmprsk.aalen. Learner defined in `mlr3proba`. Included for comparison

library(mlr3)
library(mlr3proba)
library(data.table)
cat("\n -- `example1-aalen.R` executed ...\n")
# Load and prepare the pbc task
task <- tsk("pbc")
task$select(c("age", "bili", "sex"))
cat("\n -- pbc task \n")
print(task)

# Initial partition
set.seed(123)
part <- partition(task, ratio = 0.7)

learner <- lrn("cmprsk.aalen")
learner$train(task, part$train)

cat("\n -- Model fit \n")
print(learner$model)

cat("\n -- Predicted values \n")
pred <- learner$train(task, part$train)$predict(task, part$test)
print(pred)

cat("\n -- CIF list structure \n ")
print(str(pred$cif))

cat("\n -- `pred_dt` \n")
pred_dt <- as.data.table(pred)
print(head(pred_dt))

# Expand all rows into wide format with CIF1 and CIF2 columns
wide_dt <- pred_dt[, {
  cif_list <- CIF[[1]]                              # List from each row
  .(time_point = as.numeric(names(cif_list[[1]])),  # Time points from names
    CIF1 = cif_list[[1]],                           # First vector
    CIF2 = cif_list[[2]])                           # Second vector
}, by = .(row_ids, time, event)]

cat("\n pred_dt in wide format\n")
print(wide_dt)
