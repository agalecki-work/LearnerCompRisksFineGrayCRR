# Standalone script to demonstrate competing risks analysis with the cmprsk.crr learner
# in the LearnerCompRisksFineGrayCRR package.
# Purpose: Perform a Fine-Gray competing risks regression on the pbc dataset, modeling
# cumulative incidence functions (CIFs) with fixed and time-varying covariates.
# Dataset: pbc (primary biliary cirrhosis) with 'time' (days), 'status' (0=censored,
# 1=event 1, 2=event 2), and features: age, bili, sex.
# Note: The tf function in cov2_info returns a matrix to match the number of covariates
# in cov2nms (age, sex), ensuring compatibility with cmprsk::crr requirements.

# Load required libraries
library(mlr3)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)

# Step 1: Load and configure the task
# Use the pbc dataset with default event configuration (status as event column)
# Select features: age, bili (bilirubin), sex
cat("Configuring the pbc task...\n")
task <- tsk("pbc")
task$select(c("age", "bili", "sex"))
cat("Features:", paste(task$feature_names, collapse = ", "), "\n")
cat("Targets: time =", task$target_names[1], ", event =", task$target_names[2], "\n")
cat("Observations:", task$nrow, "\n")

# Step 2: Initialize the cmprsk.crr learner
# Configure with time-varying covariates (age, sex with log(time) and log(time+1) effects)
# sex excluded from fixed effects (cov2only); set optimization parameters
cat("Initializing the cmprsk.crr learner...\n")
learner <- lrn("cmprsk.crr",
  cov2_info = list(
    cov2nms = c("age", "sex"),          # Time-varying covariates
    tf = function(uft) cbind(log(uft), log(uft + 1)),  # Return matrix with 2 columns
    cov2only = "sex"                    # Exclude sex from fixed effects
  ),
  maxiter = 100,                        # Max iterations for cmprsk::crr
  gtol = 1e-6,                          # Convergence tolerance
  parallel = FALSE                      # No parallel processing
)
cat("Learner ID:", learner$id, "\n")
cat("Predict type:", learner$predict_types, "\n")

# Step 3: Train the model
# Fit the Fine-Gray model to estimate CIFs for each competing event
cat("Training the model...\n")
learner$train(task)
cat("Model trained. Events:", paste(names(learner$model), collapse = ", "), "\n")
cat("Coefficients (event 1):", paste(names(learner$model[[1]]$coef), collapse = ", "), "\n")

# Step 4: Predict CIFs
# Generate CIFs for each observation and event at unique event times
cat("Predicting CIFs...\n")
pred <- learner$predict(task)
cat("Prediction class:", class(pred)[1], "\n")
cat("CIF rows (event 1):", nrow(pred$cif[[1]]), "\n")
cat("Time points:", ncol(pred$cif[[1]]), "\n")
cat("Sample CIFs (event 1, first 5 obs, first 5 times):\n")
print(pred$cif[[1]][1:5, 1:5])

# Step 5: Check convergence
# Verify convergence status for each event model
cat("Convergence status:\n")
print(learner$convergence())

# Step 6: Calculate variable importance
# Compute coefficient-based importance for each feature and event
cat("Variable importance:\n")
print(learner$importance())

# Step 7: Compare with direct cmprsk::crr
# Run cmprsk::crr directly for validation
cat("Running cmprsk::crr for comparison...\n")
library(cmprsk)
data <- task$data()
ftime <- data$time
fstatus <- as.numeric(data$status)
cov1 <- model.matrix(~ age + bili, data = data)[, -1]
cov2 <- model.matrix(~ age + sex, data = data)[, -1]
tf <- function(uft) cbind(log(uft), log(uft + 1))  # Match learner’s tf
model <- cmprsk::crr(ftime, fstatus, cov1, cov2, tf, failcode = 1, cencode = 0)
cat("cmprsk::crr summary (event 1):\n")
print(summary(model))

cat("Analysis completed.\n")