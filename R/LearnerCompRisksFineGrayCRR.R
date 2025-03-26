#' Fine-Gray Competing Risks Regression Learner
#'
#' @description
#' A learner for Fine-Gray competing risks regression using [cmprsk::crr()] within
#' the `mlr3proba` framework. Models cumulative incidence functions (CIFs) for
#' competing risks, supporting both fixed and time-varying covariates.
#'
#' @template param_cov2_info
#' @section Parameters:
#' \describe{
#'   \item{failcode}{`integer(1)`\cr Event code of interest (default: 1).}
#'   \item{cencode}{`integer(1)`\cr Censoring code (default: 0).}
#'   \item{maxiter}{`integer(1)`\cr Maximum iterations for convergence (default: 100).}
#' }
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' task <- tsk("pbc")
#' task$select(c("age", "bili", "sex"))
#' learner <- lrn("cmprisk.crr",
#'   cov2_info = list(
#'     cov2nms = c("age", "bili"),
#'     tfun = function(t, task) matrix(log(t), ncol = 1, dimnames = list(NULL, "logtime")),
#'     common_tfun = TRUE
#'   )
#' )
#' learner$train(task)
#' pred <- learner$predict(task)
#' print(pred)
LearnerCompRisksFineGrayCRR <- R6::R6Class("LearnerCompRisksFineGrayCRR",
  inherit = mlr3proba::LearnerCompRisks,
  public = list(
    #' @description Creates a new instance of this learner.
    #' @param cov2_info Optional list for time-varying covariates.
    initialize = function(cov2_info = NULL) {
      # cat(sprintf("Initializing learner - Dynamic Timestamp: %s\n", Sys.time()))
      ps <- paradox::ps(
        failcode = paradox::p_int(default = 1L, lower = 1L, tags = "predict"),
        cencode = paradox::p_int(default = 0L, tags = "train"),
        maxiter = paradox::p_int(default = 100L, lower = 1L, upper = 1000L, tags = "train")
      )
      ps$values <- list(failcode = 1L, cencode = 0L, maxiter = 100L)
      super$initialize(
        id = "cmprisk.crr",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = "cif",
        packages = c("mlr3proba", "cmprsk", "paradox"),
        label = "Fine-Gray CRR Model",
        man = "LearnerCompRisksFineGrayCRR::LearnerCompRisksFineGrayCRR"
      )
      if (!is.null(cov2_info)) {
        if (!is.list(cov2_info)) stop("cov2_info must be a list")
        if (!all(c("cov2nms", "tfun") %in% names(cov2_info))) {
          stop("cov2_info must contain 'cov2nms' and 'tfun'")
        }
        if (!is.character(cov2_info$cov2nms) || length(cov2_info$cov2nms) == 0) {
          stop("cov2nms must be a non-empty character vector")
        }
        if (!is.function(cov2_info$tfun)) stop("tfun must be a function")
        cov2_info$common_tfun <- if (is.null(cov2_info$common_tfun)) TRUE else cov2_info$common_tfun
        if (!is.logical(cov2_info$common_tfun)) stop("common_tfun must be logical")
      }
      private$cov2_info <- cov2_info
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
      # cat("Starting .train()\n")
      # cat(sprintf("Number of row_ids: %d\n", length(row_ids)))
      pv <- self$param_set$get_values(tags = "train")
      full_data <- task$data(rows = row_ids)
      features <- task$feature_names
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]
      # cat("Head of full_data:\n")
      # print(head(full_data))
      # cat("fstatus unique values:\n")
      # print(unique(full_data[[event_col]]))

      formula <- as.formula(paste("~", paste(features, collapse = " + ")))
      cov1 <- model.matrix(formula, data = full_data)[, -1, drop = FALSE]
      if (any(is.na(cov1))) stop("NAs detected in cov1")

      if (!is.null(private$cov2_info)) {
        cov2nms <- private$cov2_info$cov2nms
        tf <- private$cov2_info$tfun
        common_tfun <- private$cov2_info$common_tfun
        allowed_types <- c("integer", "numeric")

        for (nm in cov2nms) {
          if (!nm %in% features) {
            stop(sprintf("cov2nms element '%s' not found in task features", nm))
          }
          feature_type <- task$feature_types$type[task$feature_types$id == nm]
          if (!feature_type %in% allowed_types) {
            stop(sprintf("cov2nms element '%s' must be numeric (integer or numeric), not '%s'", nm, feature_type))
          }
        }

        cov2 <- as.matrix(full_data[, cov2nms, with = FALSE])
        uft <- unique(as.numeric(full_data[[time_col]]))

        if (common_tfun) {
          if (length(unique(cov2nms)) != length(cov2nms)) {
            stop("cov2nms must not contain repeats when common_tfun = TRUE")
          }
          tf_out <- tf(uft, task)
          if (!is.numeric(tf_out) && !is.matrix(tf_out)) {
            stop("tfun must return a numeric vector or matrix when common_tfun = TRUE")
          }
          if (is.matrix(tf_out) && ncol(tf_out) > 1) {
            stop("tfun must return a single-column matrix or vector when common_tfun = TRUE")
          }
          tf_vec <- if (is.matrix(tf_out)) tf_out[, 1] else tf_out
          if (length(tf_vec) != length(uft)) {
            stop("tfun output length must match number of unique failure times")
          }
          tf_matrix <- matrix(rep(tf_vec, length(cov2nms)), nrow = length(uft), ncol = length(cov2nms))
          tf_name <- if (is.matrix(tf_out) && !is.null(colnames(tf_out))) colnames(tf_out)[1] else "tf"
          tf_names <- paste(cov2nms, tf_name, sep = "*")
          colnames(tf_matrix) <- tf_names
          tf_final <- function(uft_new, task) {
            idx <- match(uft_new, uft)
            tf_matrix[idx, , drop = FALSE]
          }
        } else {
          tf_out <- tf(uft, task)
          if (!is.matrix(tf_out) || ncol(tf_out) != length(cov2nms)) {
            stop(sprintf("tfun must return a matrix with %d columns (length(cov2nms)) when common_tfun = FALSE", length(cov2nms)))
          }
          if (nrow(tf_out) != length(uft)) {
            stop("tfun output must have rows equal to unique failure times")
          }
          tf_final <- tf
        }
      } else {
        cov2 <- NULL
        tf_final <- NULL
      }
      if (!is.null(cov2) && any(is.na(cov2))) stop("NAs detected in cov2")

      ftime <- as.numeric(full_data[[time_col]])
      private$all_event_times <- sort(unique(ftime))
      # cat("Number of unique event times:\n")
      # cat(length(private$all_event_times), "\n")

      models <- list()
      event_times <- list()
      coefficients <- list()
      event_levels <- as.character(task$cmp_events)

      for (target_event in event_levels) {
        # cat(sprintf("Training model for event: %s\n", target_event))
        fstatus <- as.numeric(full_data[[event_col]])
        model <- tryCatch({
          if (is.null(cov2)) {
            cmprsk::crr(ftime, fstatus, cov1, failcode = as.integer(target_event), cencode = pv$cencode)
          } else {
            cmprsk::crr(ftime, fstatus, cov1, cov2, tf_final, failcode = as.integer(target_event), cencode = pv$cencode)
          }
        }, error = function(e) {
          stop(sprintf("Failed to train model for event %s: %s", target_event, e$message))
        })
        # cat("CRR model summary:\n")
        # print(summary(model))
        event_times[[target_event]] <- model$uftime
        coefficients[[target_event]] <- model$coef
        models[[target_event]] <- model
      }

      private$models <- models
      private$event_times <- event_times
      private$cov2 <- cov2
      private$tf <- tf_final
      private$feature_names <- features
      private$cov2_names <- if (!is.null(cov2)) colnames(cov2) else NULL
      invisible(self)
    },
    .predict = function(task, row_ids = task$row_ids) {
      # cat("Starting .predict()\n")
      newdata <- task$data(rows = row_ids)
      event_levels <- as.character(task$cmp_events)
      cif_list <- vector("list", length(event_levels))
      names(cif_list) <- event_levels

      formula <- as.formula(paste("~", paste(task$feature_names, collapse = " + ")))
      cov1 <- model.matrix(formula, data = newdata)[, -1, drop = FALSE]
      if (any(is.na(cov1))) stop("NAs detected in cov1")

      if (!is.null(private$cov2_info)) {
        cov2nms <- private$cov2_info$cov2nms
        cov2 <- as.matrix(newdata[, cov2nms, with = FALSE])
        tf <- private$tf
      } else {
        cov2 <- NULL
        tf <- NULL
      }
      if (!is.null(cov2) && any(is.na(cov2))) stop("NAs detected in cov2")

      all_times <- private$all_event_times
      all_times_char <- as.character(all_times)
      # cat("Number of requested time points:\n")
      # cat(length(all_times), "\n")

      for (event in event_levels) {
        # cat(sprintf("Predicting for event: %s\n", event))
        model <- private$models[[event]]
        if (is.null(model)) stop(sprintf("No model available for event %s", event))
        pred <- tryCatch({
          pred_raw <- predict(model, cov1 = cov1, cov2 = cov2, tf = tf, time = all_times)
          pred_times <- pred_raw[, 1]
          pred_cif <- t(pred_raw[, -1, drop = FALSE])
          n_times <- length(all_times)
          if (ncol(pred_cif) != n_times) {
            # cat("Adjusting CIF to match all_times\n")
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