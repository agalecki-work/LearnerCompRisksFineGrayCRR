library(mlr3)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)  # Explicitly load the package
library(testthat)

test_that("LearnerCompRisksFineGrayCRR passes autotest", {
  skip_if_not_installed("cmprsk")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("survival")
  skip_if(!exists("run_autotest", envir = asNamespace("mlr3")), "run_autotest not available in mlr3")
  learner = mlr3::lrn("cmprsk.crr", maxiter = 100, gtol = 1e-6, parallel = FALSE,
                      cov2_info = NULL, init_list = NULL, censor_group = NULL)
  expect_silent(result <- mlr3::run_autotest(learner))
  expect_true(is.list(result))
  expect_true(all(sapply(result, function(x) x$passed)))
})

test_that("Task configuration is correct", {
  skip_if_not_installed("mlr3proba")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  expect_equal(task$target_names, c("time", "status"))
  expect_equal(task$col_roles$stratum, "status")
  expect_true(is.integer(task$data(cols = "status")$status))
})

test_that("Class and ID checks", {
  skip_if_not_installed("mlr3proba")
  learner <- lrn("cmprsk.crr")
  expect_s3_class(learner, "LearnerCompRisks")
  expect_true(exists("new", envir = mlr3proba::LearnerCompRisks))
  expect_equal(learner$id, "cmprsk.crr")
  expect_equal(learner$predict_types, c("cif"))
  expect_true(all(c("importance", "missings") %in% learner$properties))
})

test_that("Training and prediction with no cov2_info", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner <- lrn("cmprsk.crr")
  expect_silent(learner$train(task, part$train))
  expect_true(exists("PredictionCompRisks",
                     envir = asNamespace("mlr3proba"),
                     inherits = FALSE))
  pred <- learner$predict(task, part$test)
  expect_s3_class(pred, "PredictionCompRisks")
  expect_equal(names(pred$cif), as.character(task$cmp_events))
})

test_that("Numeric predictors with two-column tf matrix", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner_numeric <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) cbind(log(uft), log(uft + 1))
    )
  )
  expect_silent(learner_numeric$train(task, part$train))
  pred_numeric <- learner_numeric$predict(task, part$test)
  expect_s3_class(pred_numeric, "PredictionCompRisks")
  expect_equal(names(pred_numeric$cif), as.character(task$cmp_events))
})

test_that("Mixed numeric and factor variables with two-column tf matrix", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner_mixed <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "sex"),
      tf = function(uft) cbind(log(uft), log(uft + 1))
    )
  )
  expect_silent(learner_mixed$train(task, part$train))
  pred_mixed <- learner_mixed$predict(task, part$test)
  expect_s3_class(pred_mixed, "PredictionCompRisks")
  expect_equal(names(pred_mixed$cif), as.character(task$cmp_events))
})

test_that("Repeats in cov2nms with two-column tf matrix", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner_repeats <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "age"),
      tf = function(uft) cbind(log(uft), uft)
    )
  )
  expect_silent(learner_repeats$train(task, part$train))
  pred_repeats <- learner_repeats$predict(task, part$test)
  expect_s3_class(pred_repeats, "PredictionCompRisks")
  expect_equal(names(pred_repeats$cif), as.character(task$cmp_events))
})

test_that("cov2only with bili as time-varying only", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner_cov2only <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) cbind(log(uft), uft),
      cov2only = c("bili")
    )
  )
  expect_silent(learner_cov2only$train(task, part$train))
  pred_cov2only <- learner_cov2only$predict(task, part$test)
  expect_s3_class(pred_cov2only, "PredictionCompRisks")
  expect_type(learner_cov2only$state$model, "list")
  expect_length(learner_cov2only$state$model, length(task$cmp_events))
  expect_s3_class(learner_cov2only$state$model[[1]], "crr")
  coef_names <- names(learner_cov2only$state$model[[1]]$coef)
  cov1_names <- coef_names[!grepl("\\*", coef_names)]
  cov2_names <- coef_names[grepl("\\*", coef_names)]
  expect_false("bili" %in% cov1_names, "bili not in cov1 with cov2only")
  expect_true(any(grepl("bili", cov2_names)), "bili should be in cov2 effects")
})

test_that("Convergence method", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner <- lrn("cmprsk.crr")
  expect_silent(learner$train(task, part$train))
  expect_type(learner$state$model, "list")
  expect_length(learner$state$model, length(task$cmp_events))
  expect_true(all(sapply(learner$convergence(), is.logical)))
})

test_that("Importance method", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner <- lrn("cmprsk.crr")
  expect_silent(learner$train(task, part$train))
  for (cause in as.character(task$cmp_events)) {
    imp <- learner$importance(cause = cause)
    expect_true(is.numeric(imp) || is.null(imp))
    if (!is.null(imp)) {
      expect_true(all(imp >= 0))
      expect_equal(sum(imp), 1, tolerance = 1e-6)
      expect_true(all(names(imp) %in% c("age", "bili", "sexf")))
    }
  }
})

test_that("Single predictor", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  task_single <- task$clone()
  task_single$select(c("age"))
  learner_single <- lrn("cmprsk.crr")
  expect_silent(learner_single$train(task_single, part$train))
  pred_single <- learner_single$predict(task_single, part$test)
  expect_s3_class(pred_single, "PredictionCompRisks")
})

test_that("No features", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  task_nofeat <- task$clone()
  task_nofeat$select(character(0))
  learner_nofeat <- lrn("cmprsk.crr")
  expect_error(learner_nofeat$train(task_nofeat, part$train),
               "system is exactly singular")
})

test_that("Invalid cov2_info (non-existent feature)", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner_invalid <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("invalid"),
      tf = function(uft) log(uft)
    )
  )
  expect_error(learner_invalid$train(task, part$train),
               "cov2nms element 'invalid' not in task features")
})

test_that("Mismatched tf output dimensions", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner_mismatch <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) matrix(log(uft), ncol = 1)
    )
  )
  expect_error(learner_mismatch$train(task, part$train),
               "tf must return a matrix with")
})

test_that("Empty task", {
  skip_if_not_installed("mlr3proba")
  task <- configure_task(features = c(character(0)), stratum = "status")
  part <- create_partition(task)
  task_empty <- task$clone()
  expect_error(task_empty$filter(integer(0)), "competing event\\(s\\)")
})

test_that("Parameter validation", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner <- lrn("cmprsk.crr")
  expected_params <- c("maxiter", "gtol", "parallel", "cov2_info")
  expect_true(all(expected_params %in% learner$param_set$ids()))
  expect_silent(learner$param_set$assert(learner$param_set$values))
  learner$param_set$values$maxiter <- 50L
  learner$param_set$values$gtol <- 1e-7
  learner$param_set$values$parallel <- FALSE
  learner$param_set$values$cov2_info <- list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), log(uft + 1)),
    cov2only = NULL
  )
  expect_silent(learner$param_set$assert(learner$param_set$values))
  expect_silent(learner$train(task, part$train))
  pred <- learner$predict(task, part$test)
  expect_s3_class(pred, "PredictionCompRisks")
})

test_that("Parallel execution", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  skip_if_not_installed("future.apply")
  task <- configure_task(features = c("age", "bili", "sex"), stratum = "status")
  part <- create_partition(task)
  learner_parallel <- lrn("cmprsk.crr", parallel = TRUE)
  expect_silent(learner_parallel$train(task, part$train))
  pred_parallel <- learner_parallel$predict(task, part$test)
  expect_s3_class(pred_parallel, "PredictionCompRisks")
})

test_that("LearnerCompRisksFineGrayCRR works with init_list", {
  skip_if_not_installed("cmprsk")
  task <- tsk("pbc")
  task$set_col_roles(cols = "status", add_to = "stratum")
  task$col_roles$feature <- setdiff(task$feature_names, "status")
  task$select(c("age", "sex", "bili"))
  part <- partition(task, ratio = 0.7)

  # Three covariates: age, sex, bili (fixed); no cov2
  learner <- lrn("cmprsk.crr",
    maxiter = 100,
    gtol = 1e-6,
    parallel = FALSE,
    init_list = list("1" = c(0.1, 0.2, 0.3), "2" = c(0.1, 0.2, 0.3))
  )
  learner$train(task, row_ids = part$train)
  expect_true(is.list(learner$model) && length(learner$model) >= 1)
  expect_true(all(unlist(learner$convergence())))

  pred <- learner$predict(task, row_ids = part$test)
  expect_s3_class(pred, "PredictionCompRisks")
  expect_true(nrow(pred$cif[["1"]]) == length(part$test))

  # Test invalid init_list
  expect_error(
    {
      learner <- lrn("cmprsk.crr", init_list = list("1" = c(0.1, 0.2))) # Wrong length
      learner$train(task)
    },
    "init_list for event 1 must have 3 values"
  )
  expect_error(
    {
      learner <- lrn("cmprsk.crr", init_list = list("1" = c(0.1, 0.2, NA))) # Contains NA
      learner$train(task)
    },
    "init_list for event 1 must be a numeric vector with no NAs"
  )
  expect_error(
    {
      learner <- lrn("cmprsk.crr", init_list = list("3" = c(0.1, 0.2, 0.3))) # Wrong event
      learner$train(task)
    },
    "init_list must have entries for all events in task\\$cmp_events"
  )
})

library(testthat)

test_that("cengroup extracted from task$backend works", {
  task <- tsk("pbc")
  feat = c("age", "bili", "sex")
  task$select(feat)
  task$set_col_roles(cols = "status", add_to = "stratum")
  learner <- lrn("cmprsk.crr", censor_group = "edema")
  expect_silent(learner$train(task))
  expect_true(all(names(learner$state$convergence) %in% task$cmp_events))
})



test_that("cengroup NULL works", {
  task <- tsk("pbc")
  feat = c("age", "bili", "sex")
  task$select(feat)
  learner <- lrn("cmprsk.crr", censor_group = NULL)
  expect_silent(learner$train(task))
})

test_that("cengroup with invalid column fails", {
  task <- tsk("pbc")
  feat = c("age", "bili", "sex")
  task$select(feat)
  learner <- lrn("cmprsk.crr", censor_group = "invalid_col")
  expect_error(learner$train(task), "censor_group column 'invalid_col' not found in task data")
})

test_that("resampling with stratum is unaffected", {
  task <- tsk("pbc")
  feat = c("age", "bili", "sex")
  task$select(feat)
  task$set_col_roles(cols = "status", add_to = "stratum")
  learner <- lrn("cmprsk.crr", censor_group = "edema")
  resampling <- rsmp("cv", folds = 3)
  rr <- resample(task, learner, resampling)
  expect_true(is.environment(rr$prediction()))
})

test_that("prediction works after training", {
  task <- tsk("pbc")
  feat = c("age", "bili", "sex")
  task$select(feat)
  learner <- lrn("cmprsk.crr", censor_group = "edema")
  learner$train(task)
  pred <- learner$predict(task)
  expect_true(inherits(pred, "PredictionCompRisks"))
})
