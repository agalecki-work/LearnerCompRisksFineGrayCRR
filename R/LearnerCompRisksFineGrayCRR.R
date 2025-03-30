#' Fine-Gray Competing Risks Regression Learner
#'
#' @description
#' A learner implementing the Fine-Gray competing risks regression model using 
#' [cmprsk::crr()] within the `mlr3proba` framework. This learner estimates 
#' cumulative incidence functions (CIFs) for competing risks scenarios, where 
#' multiple mutually exclusive events may occur. It supports both fixed covariates 
#' and time-varying covariates through a flexible transformation function. During 
#' training, the learner fits a separate model for each event type specified in 
#' the task’s `cmp_events`, using the task’s `status` column to distinguish events 
#' from censoring (where the censoring code is conventionally set to 0 in the task 
#' definition). Predictions are generated for all event types across all unique 
#' event times observed in the training data.
#'
#' @param cov2_info `list()`\cr Optional configuration for time-varying covariates, 
#' enabling the learner to model covariate effects that change over time. This 
#' list must contain the following elements:
#'   \describe{
#'     \item{cov2nms}{`character()`\cr A vector of covariate names from the task’s 
#'       feature set that should be treated as time-varying. These must be features 
#'       available in the task at training time.}
#'     \item{tf}{`function(uft)`\cr A user-defined function specifying how the 
#'       covariates in `cov2nms` vary over time. It takes one argument: `uft` (a 
#'       numeric vector of unique failure times from the training data). The 
#'       function’s behavior is described in the [cmprsk::crr()] documentation and 
#'       must return a matrix with:
#'       \itemize{
#'         \item `nrow = length(uft)` (matching the number of unique failure times).
#'         \item `ncol` equal to the number of columns in the `cov2` matrix (derived 
#'           from `cov2nms` via `model.matrix`), where each column corresponds to a 
#'           time-varying effect for each column in `cov2`.
#'       }
#'       Example: `function(uft) matrix(log(uft), ncol = 1)` applies a logarithmic 
#'       transformation when `cov2` has one column.}
#'   }
#'   If `cov2_info` is `NULL` (default), the learner treats all covariates as fixed.
#'
#' @section Parameters:
#' \describe{
#'   \item{maxiter}{`integer(1)`\cr Maximum number of iterations for the 
#'     `cmprsk::crr()` optimization algorithm to converge. Default is 100, with 
#'     a valid range of 1 to 1000. Increase this value if convergence issues arise 
#'     with complex datasets.}
#' }
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' # Load the PBC dataset as a competing risks task
#' task <- tsk("pbc")
#' task$select(c("age", "bili", "sex"))
#' # Define a learner with time-varying covariates
#' learner <- lrn("cmprisk.crr",
#'   cov2_info = list(
#'     cov2nms = c("age", "sex"),
#'     tf = function(uft) cbind(log(uft), log(uft + 1))  # Matches ncol(cov2) = 2
#'   )
#' )
#' # Train the learner
#' learner$train(task)
#' # Predict CIFs for all event types
#' pred <- learner$predict(task)
#' print(pred)
LearnerCompRisksFineGrayCRR <- R6::R6Class("LearnerCompRisksFineGrayCRR",
  inherit = mlr3proba::LearnerCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    #' @param cov2_info See main description for details.
    initialize = function(cov2_info = NULL) {
      if (!is.null(cov2_info)) {
        if (!is.list(cov2_info)) stop("cov2_info must be a list")
        if (!all(c("cov2nms", "tf") %in% names(cov2_info))) {
          stop("cov2_info must contain 'cov2nms' and 'tf'")
        }
        if (!is.character(cov2_info$cov2nms) || length(cov2_info$cov2nms) == 0) {
          stop("cov2nms must be a non-empty character vector")
        }
        if (!is.function(cov2_info$tf)) stop("tf must be a function")
      }
      private$cov2_info <- cov2_info

      ps <- paradox::ps(
        maxiter = paradox::p_int(default = 100L, lower = 1L, upper = 1000L, tags = "train")
      )
      ps$values <- list(maxiter = 100L)

      super$initialize(
        id = "cmprisk.crr",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = "cif",
        packages = c("mlr3proba", "cmprsk", "paradox"),
        label = "Fine-Gray CRR Model",
        man = "LearnerCompRisksFineGrayCRR::LearnerCompRisksFineGrayCRR"
      )
    }
  ),
  private = list(
    models = NULL,
    event_times = NULL,
    coefficients = NULL,
    cov2 = NULL,
    tf = NULL,
    feature_names = NULL,
    cov2_names = NULL,
    cov2_info = NULL,
    all_event_times = NULL,
    .train = function(task, row_ids = task$row_ids) {
      pv <- self$param_set$get_values(tags = "train")
      full_data <- task$data(rows = row_ids)
      features <- task$feature_names
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]

      formula <- as.formula(paste("~", paste(features, collapse = " + ")))
      cov1 <- model.matrix(formula, data = full_data)[, -1, drop = FALSE]
      if (any(is.na(cov1))) stop("NAs detected in cov1")

      if (!is.null(private$cov2_info)) {
        cov2nms <- private$cov2_info$cov2nms
        tf <- private$cov2_info$tf

        for (nm in cov2nms) {
          if (!nm %in% features) {
            stop(sprintf("cov2nms element '%s' not found in task features", nm))
          }
        }

        # Create cov2 by binding model.matrix results for each feature in cov2nms
        cov2_list <- lapply(cov2nms, function(nm) {
          model.matrix(as.formula(paste("~", nm)), data = full_data)[, -1, drop = FALSE]
        })
        cov2 <- do.call(cbind, cov2_list)
        if (ncol(cov2) == 1) cov2 <- as.vector(cov2)  # Convert to vector if only one column
        uft <- unique(as.numeric(full_data[[time_col]]))

        tf_out <- tf(uft)
        if (!is.matrix(tf_out)) {
          stop("tf must return a matrix")
        }
        if (nrow(tf_out) != length(uft)) {
          stop("tf output must have rows equal to unique failure times")
        }
        if (ncol(tf_out) != ncol(as.matrix(cov2))) {
          stop(sprintf("tf must return a matrix with %d columns (matching cov2 columns)", ncol(as.matrix(cov2))))
        }
        tf_final <- tf
      } else {
        cov2 <- NULL
        tf_final <- NULL
      }
      if (!is.null(cov2) && any(is.na(cov2))) stop("NAs detected in cov2")

      ftime <- as.numeric(full_data[[time_col]])
      private$all_event_times <- sort(unique(ftime))

      models <- list()
      event_times <- list()
      coefficients <- list()
      event_levels <- as.character(task$cmp_events)

      for (target_event in event_levels) {
        fstatus <- as.numeric(full_data[[event_col]])
        model <- tryCatch({
          if (is.null(cov2)) {
            cmprsk::crr(ftime, fstatus, cov1, failcode = as.integer(target_event), cencode = 0L)
          } else {
            cmprsk::crr(ftime, fstatus, cov1, cov2, tf_final, failcode = as.integer(target_event), cencode = 0L)
          }
        }, error = function(e) {
          stop(sprintf("Failed to train model for event %s: %s", target_event, e$message))
        })
        event_times[[target_event]] <- model$uftime
        coefficients[[target_event]] <- model$coef
        models[[target_event]] <- model
      }

      private$models <- models
      private$event_times <- event_times
      private$cov2 <- cov2
      private$tf <- tf_final
      private$feature_names <- features
      private$cov2_names <- if (!is.null(cov2)) colnames(as.matrix(cov2)) else NULL
      invisible(self)
    },
    .predict = function(task, row_ids = task$row_ids) {
      newdata <- task$data(rows = row_ids)
      event_levels <- as.character(task$cmp_events)
      cif_list <- vector("list", length(event_levels))
      names(cif_list) <- event_levels

      formula <- as.formula(paste("~", paste(task$feature_names, collapse = " + ")))
      cov1 <- model.matrix(formula, data = newdata)[, -1, drop = FALSE]
      if (any(is.na(cov1))) stop("NAs detected in cov1")

      if (!is.null(private$cov2_info)) {
        cov2nms <- private$cov2_info$cov2nms
        # Create cov2 for prediction using the same logic as in training
        cov2_list <- lapply(cov2nms, function(nm) {
          model.matrix(as.formula(paste("~", nm)), data = newdata)[, -1, drop = FALSE]
        })
        cov2 <- do.call(cbind, cov2_list)
        if (ncol(cov2) == 1) cov2 <- as.vector(cov2)  # Convert to vector if only one column
        tf <- private$tf
      } else {
        cov2 <- NULL
        tf <- NULL
      }
      if (!is.null(cov2) && any(is.na(cov2))) stop("NAs detected in cov2")

      all_times <- private$all_event_times
      all_times_char <- as.character(all_times)

      for (event in event_levels) {
        model <- private$models[[event]]
        if (is.null(model)) stop(sprintf("No model available for event %s", event))
        pred <- tryCatch({
          pred_raw <- predict(model, cov1 = cov1, cov2 = cov2, tf = tf, time = all_times)
          pred_times <- pred_raw[, 1]
          pred_cif <- t(pred_raw[, -1, drop = FALSE])
          n_times <- length(all_times)
          if (ncol(pred_cif) != n_times) {
            full_cif <- matrix(0, nrow = nrow(pred_cif), ncol = n_times)
            colnames(full_cif) <- all_times_char
            time_idx <- match(pred_times, all_times)
            if (any(is.na(time_idx))) stop("Predicted times do not match training times")
            for (i in seq_len(nrow(pred_cif))) {
              for (j in seq_len(ncol(pred_cif))) {
                full_cif[i, time_idx[j]:n_times] <- pred_cif[i, j]
              }
            }
            pred_cif <- full_cif
          }
          colnames(pred_cif) <- all_times_char
          rownames(pred_cif) <- NULL
          pred_cif
        }, error = function(e) {
          stop(sprintf("Failed to predict for event %s: %s", event, e$message))
        })
        cif_list[[event]] <- pred
      }

      list(cif = cif_list)
    }
  )
)