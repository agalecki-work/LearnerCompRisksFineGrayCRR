# Load required libraries
library(mlr3)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)

library(testthat)

# Define the task
task <- tsk("pbc")
task$select(c("age", "bili", "sex"))

# Test suite for cmprsk.crr learner
test_that("cmprsk.crr works with pbc task", {
  
  # 1. Learner Initialization
  learner <- lrn("cmprsk.crr")
  expect_s3_class(learner, "LearnerCompRisks")
  expect_true("cif" %in% learner$predict_types, info = "Should support crank predict type")
  expect_true(all(c("logical", "integer", "numeric", "factor") %in% learner$feature_types), 
              info = "Should support integer, double, and factor features")
  
  # 2. Hyperparameter Handling
  learner$param_set$values$maxiter <- 101 # Event of interest (e.g., death)
  expect_equal(learner$param_set$values$maxiter, 101, info = "maxiter should be set to 101")
 
  expect_error(learner$param_set$values$maxiter <- "invalid" , info = "Should error on invalid maxiter type")
  
  # 3. Training
  expect_silent(learner$train(task))
  expect_true(!is.null(learner$model), info = "Model should be created")
  expect_type(learner$model, "list")
  expect_equal(sort(names(learner$model[[1]]$coef)), sort(c("age", "bili", "sexf")), 
               info = "Model coefficients should match task features")
  
  # 4. Prediction
  pred <- learner$predict(task)
  expect_s3_class(pred, "PredictionCompRisks")
  expect_equal(nrow(pred$cif[[1]]), 276, info = "Prediction nrow in cif matrix should match task rows (276)")
  # CIF matrix for competing event 2 (first 5 test observations and 10 time points)
  expect_equal(dim(pred$cif[[2]][1:5, 1:10]), c(5, 10), info = "CIF matrix should have 5 rows and 10 columns") 
  
  # List of cif vectors for the second test observation
  dt = as.data.table(pred)
  expect_type(dt$CIF[[2]], "list")
  expect_equal(length(dt$CIF[[2]][[1]]), 123, info = "cif vector should have length of 123")
  
  # 5. Missing data (introduce NA in age)
  dt = task$data()
  dt[1, age := NA]
  task_na <- TaskCompRisks$new(id = paste0(task$id, "_na"), backend = dt, time = "time", event = "status") 
  expect_error(learner$train(task_na), "missing values", 
               info = "Should error or handle NA values gracefully")
  
  # 6. Resampling
  resampling <- rsmp("cv", folds = 3)
  rr <- resample(task, learner, resampling)
  expect_type(rr, "environment")
})

