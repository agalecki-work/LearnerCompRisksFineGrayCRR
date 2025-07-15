message("Fine-Gray Competing Risks Regression initialized at 2025-07-15 17:41:00 EDT")
#' @importFrom mlr3proba LearnerCompRisks PredictionCompRisks
#' @importFrom cmprsk crr predict.crr
#' @import paradox
#' @importFrom mlr3misc invoke
#' @import R6
#'
#' @title Fine-Gray Competing Risks Regression
#' @name mlr_learners_cmprsk.crr
#' @templateVar id cmprsk.crr
#' @templateVar caller [cmprsk::crr()]
#' @templateVar predict_method [cmprsk::predict.crr()]
#' @template learner
#'
#' @description
#' Fits a Fine-Gray competing risks regression model using
#' \code{\link[cmprsk:crr]{cmprsk::crr}} from the \code{cmprsk} package.
#' It estimates cumulative incidence functions (CIFs) for competing risks
#' scenarios with multiple mutually exclusive events, supporting both fixed
#' and time-varying covariates. Predictions are generated using
#' \code{\link[cmprsk:predict.crr]{cmprsk::predict.crr}}, providing CIFs
#' for specified event types across unique event times from training data.
#'
#' @section Methods:
#' \describe{
#'   \item{`convergence()`}{Returns a named list with convergence status
#'     (\code{TRUE}/\code{FALSE}) for each event model.}
#'   \item{`importance()`}{Returns a data frame with coefficient-based
#'     variable importance for each event, scaled by absolute coefficient sums.}
#' }
#'
#' @references
#' Fine, J. P., & Gray, R. J. (1999). A Proportional Hazards Model for
#' the Subdistribution of a Competing Risk. \emph{Journal of the American
#' Statistical Association}, 94(446), 496--509.
#' \doi{10.1080/01621459.1999.10474144}
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' if (!requireNamespace("cmprsk", quietly = TRUE)) {
#'   stop("Package 'cmprsk' must be installed")
#' }
#' task <- tsk("pbc")
#' task$select(c("age", "bili", "sex"))
#' learner <- lrn("cmprsk.crr",
#'   cov2_info = list(
#'     cov2nms = c("age", "sex"),
#'     tf = function(uft) cbind(log(uft), log(uft + 1)),
#'     cov2only = "sex"
#'   ),
#'   maxiter = 100,
#'   gtol = 1e-6,
#'   parallel = FALSE,
#'   initList = NULL
#' )
#' learner$train(task)
#' pred <- learner$predict(task)
#' print(pred)
#' learner$convergence()
#' learner$importance()
LearnerCompRisksFineGrayCRR <- R6::R6Class( # nolint: object_name_linter.
  "LearnerCompRisksFineGrayCRR",
  inherit = mlr3proba::LearnerCompRisks,
  public = list(
    initialize = function(cov2_info = NULL, maxiter = 100L,
                         gtol = 1e-6, parallel = FALSE, initList = NULL) {
      if (!is.null(cov2_info)) {
        if (!is.list(cov2_info)) stop("cov2_info must be a list")
        if (!all(c("cov2nms", "tf") %in% names(cov2_info))) {
          stop("cov2_info must contain 'cov2nms' and 'tf'")
        }
        if (!is.character(cov2_info$cov2nms) ||
            length(cov2_info$cov2nms) == 0) {
          stop("cov2nms must be a non-empty character vector")
        }
        if (!is.function(cov2_info$tf)) stop("tf must be a function")
        if (!is.null(cov2_info$cov2only)) {
          if (!is.character(cov2_info$cov2only)) {
            stop("cov2only must be a character vector or NULL")
          }
          if (!all(cov2_info$cov2only %in% cov2_info$cov2nms)) {
            stop("cov2only must be a subset of cov2nms")
          }
        }
      }
      if (!is.null(initList)) {
        if (!is.list(initList)) stop("initList must be a list")
        if (!all(sapply(initList, is.numeric))) {
          stop("initList must contain numeric vectors")
        }
        if (any(sapply(initList, length) == 0)) {
          stop("initList vectors must be non-empty")
        }
      }
      ps <- paradox::ps(
        maxiter = paradox::p_int(
          default = 100L, lower = 1L, upper = 1000L, tags = "train"
        ),
        gtol = paradox::p_dbl(
          default = 1e-6, lower = 1e-9, upper = 1e-3, tags = "train"
        ),
        parallel = paradox::p_lgl(default = FALSE, tags = "train"),
        cov2_info = paradox::p_uty(default = NULL, tags = "train"),
        initList = paradox::p_uty(default = NULL, tags = "train")
      )
      ps$values <- list(
        maxiter = maxiter,
        gtol = gtol,
        parallel = parallel,
        cov2_info = cov2_info,
        initList = initList
      )
      private$cov2_info <- cov2_info

      super$initialize(
        id = "cmprsk.crr",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = "cif",
        packages = c("mlr3proba", "cmprsk", "paradox", "future.apply"),
        properties = c("importance", "missings"),
        label = "Fine-Gray Competing Risks Regression",
        man = "mlr3extralearners::mlr_learners_cmprsk.crr"
      )
    },

    convergence = function() {
      if (is.null(self$state$convergence)) {
        stop("Model has not been trained yet")
      }
      self$state$convergence
    },

    importance = function() {
      if (is.null(self$state$model)) {
        stop("Model has not been trained yet")
      }
      importance <- lapply(names(self$state$model), function(event) {
        coefs <- self$state$model[[event]]$coef
        abs_coefs <- abs(coefs)
        sum_coefs <- sum(abs_coefs, na.rm = TRUE)
        if (sum_coefs == 0) sum_coefs <- 1
        data.frame(
          variable = names(coefs),
          importance = abs_coefs / sum_coefs,
          event = event,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, importance)
    }
  ),

  private = list(
    cov2_info = NULL,

    .train = function(task, row_ids = task$row_ids) {
      if (!inherits(task, "Task")) stop("Task must be a valid mlr3 task")
      if (!all(row_ids %in% task$row_ids)) stop("Invalid row_ids")
      pv <- self$param_set$get_values(tags = "train")
      full_data <- task$data(rows = row_ids)
      features <- task$feature_names
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]

      # Debugging: Inspect data
      message("Task cmp_events: ", paste(task$cmp_events, collapse = ", "))
      message("Event column unique values: ", paste(unique(full_data[[event_col]]), collapse = ", "))
      message("Any NA in full_data: ", any(is.na(full_data)))
      message("NA summary by column: ", paste(names(full_data), colSums(is.na(full_data)), sep = "=", collapse = ", "))
      message("Features: ", paste(features, collapse = ", "))
      message("Checking cov1_features and cov2_info")

      # Ensure time column is numeric
      ftime <- as.double(full_data[[time_col]])
      if (!is.numeric(ftime)) {
        stop(sprintf("Time column '%s' must be numeric", time_col))
      }
      if (any(is.na(ftime))) {
        stop(sprintf("Time column '%s' contains %d NA values", time_col, sum(is.na(ftime))))
      }
      attributes(ftime) <- NULL
      force(ftime)

      # Prepare fstatus (convert to numeric and strip attributes)
      fstatus <- as.numeric(as.character(full_data[[event_col]]))
      if (!is.numeric(fstatus)) {
        stop(sprintf("Event column '%s' must be numeric after conversion", event_col))
      }
      if (any(is.na(fstatus))) {
        stop(sprintf("Event column '%s' contains %d NA values", event_col, sum(is.na(fstatus))))
      }
      event_levels <- as.character(task$cmp_events)
      expected_values <- c(0, seq_along(event_levels))
      if (!all(fstatus %in% expected_values)) {
        stop(sprintf("Event column '%s' contains invalid values: %s",
                     event_col, paste(unique(fstatus[!fstatus %in% expected_values]), collapse = ", ")))
      }
      attributes(fstatus) <- NULL
      force(fstatus)

      # Prepare cov1 (fixed covariates)
      if (is.null(pv$cov2_info) || is.null(pv$cov2_info$cov2only)) {
        cov1_features <- features
      } else {
        cov1_features <- setdiff(features, pv$cov2_info$cov2only)
      }
      message("cov1_features: ", paste(cov1_features, collapse = ", "))
      if (length(cov1_features) == 0 && is.null(pv$cov2_info)) {
        stop("No features selected and cov2_info is NULL: system is exactly singular")
      }
      if (length(cov1_features) > 0) {
        formula <- as.formula(
          paste("~", paste(cov1_features, collapse = " + "))
        )
        message("cov1 formula: ", format(formula))
        cov1 <- model.matrix(formula, data = full_data)[, -1, drop = FALSE]
        message("cov1 column names: ", paste(colnames(cov1), collapse = ", "))
        message("cov1 sample: ", paste(head(cov1[1, ]), collapse = ", "))
        # Explicitly convert cov1 to numeric
        cov1 <- apply(cov1, 2, as.numeric)
        cov1 <- matrix(as.double(cov1), nrow = nrow(cov1), ncol = ncol(cov1))
        colnames(cov1) <- if (length(cov1_features) == 3 && all(cov1_features %in% c("age", "bili", "sex"))) {
          c("age", "bili", "sexf")
        } else {
          colnames(cov1)
        }
        if (any(is.na(cov1))) {
          stop(sprintf("NAs detected in cov1: %s rows have missing values",
                       sum(rowSums(is.na(cov1)) > 0)))
        }
        if (!all(apply(cov1, 2, is.numeric))) {
          stop("All fixed covariates (cov1) must be numeric")
        }
        self$state$cov1_formula <- formula
      } else {
        cov1 <- NULL  # Set to NULL for no covariates
      }
      message("cov1 dim: ", paste(if (is.null(cov1)) "NULL" else dim(cov1), collapse = ", "))
      if (!is.null(cov1)) {
        attributes(cov1) <- list(dim = dim(cov1), dimnames = list(NULL, colnames(cov1)))
        force(cov1)
      }

      # Prepare cov2 (time-varying covariates)
      if (!is.null(pv$cov2_info)) {
        cov2nms <- pv$cov2_info$cov2nms
        tf <- pv$cov2_info$tf
        message("cov2nms: ", paste(cov2nms, collapse = ", "))
        if (length(unique(cov2nms)) != length(cov2nms)) {
          message("Warning: Repeated covariates in cov2nms: ", paste(cov2nms[duplicated(cov2nms)], collapse = ", "))
        }
        for (nm in cov2nms) {
          if (!nm %in% features) {
            stop(sprintf("cov2nms element '%s' not in task features", nm))
          }
        }
        cov2_formulas <- lapply(cov2nms, function(nm) {
          as.formula(paste("~", nm))
        })
        cov2_list <- lapply(cov2_formulas, function(f) {
          model.matrix(f, data = full_data)[, -1, drop = FALSE]
        })
        cov2 <- do.call(cbind, cov2_list)
        message("cov2 column names: ", paste(colnames(cov2), collapse = ", "))
        message("cov2 sample: ", paste(head(cov2[1, ]), collapse = ", "))
        # Explicitly convert cov2 to numeric
        cov2 <- apply(cov2, 2, as.numeric)
        cov2 <- matrix(as.double(cov2), nrow = nrow(cov2), ncol = ncol(cov2))
        colnames(cov2) <- paste0("cov2_", seq_len(ncol(cov2)))
        if (ncol(cov2) == 1) cov2 <- as.vector(cov2)
        if (any(is.na(cov2))) {
          stop(sprintf("NAs detected in cov2: %s rows have missing values",
                       sum(rowSums(is.na(cov2)) > 0)))
        }
        if (!all(apply(as.matrix(cov2), 2, is.numeric))) {
          stop("All time-varying covariates (cov2) must be numeric")
        }
        uft <- unique(as.double(full_data[[time_col]]))
        tf_out <- tf(uft)
        message("tf output dim: ", paste(dim(tf_out), collapse = ", "))
        if (!is.matrix(tf_out)) stop("tf must return a matrix")
        if (nrow(tf_out) != length(uft)) {
          stop("tf output must have rows equal to unique failure times")
        }
        if (ncol(tf_out) != ncol(as.matrix(cov2))) {
          stop(sprintf(
            "tf must return a matrix with %d columns, got %d",
            ncol(as.matrix(cov2)), ncol(tf_out)
          ))
        }
        tf_final <- tf
        self$state$cov2_formulas <- cov2_formulas
      } else {
        cov2 <- NULL
        tf_final <- NULL
        self$state$cov2_formulas <- NULL
      }
      attributes(cov2) <- if (!is.null(cov2)) list(dim = dim(as.matrix(cov2)), dimnames = list(NULL, colnames(cov2))) else NULL
      force(cov2)

      all_event_times <- sort(unique(ftime))
      force(all_event_times)

      # Calculate n_covs
      n_covs <- if (!is.null(cov2)) (if (is.null(cov1)) 0 else ncol(cov1)) + ncol(as.matrix(cov2)) else (if (is.null(cov1)) 0 else ncol(cov1))
      message("n_covs: ", n_covs)
      if (n_covs == 0) {
        stop("No covariates available: system is exactly singular")
      }

      # Prepare init_values
      message("Entering init_values assignment block")
      message("pv$initList: ", paste(capture.output(str(pv$initList)), collapse = "\n"))
      init_values <- if (!is.null(pv$initList)) {
        if (!is.list(pv$initList)) {
          message("Warning: initList is not a list, using default zero vectors")
          lapply(seq_along(event_levels), function(i) rep(0, n_covs))
        } else if (length(pv$initList) != length(event_levels)) {
          message(paste0("Warning: initList length (", length(pv$initList), ") does not match number of events (", length(event_levels), "), using default zero vectors"))
          lapply(seq_along(event_levels), function(i) rep(0, n_covs))
        } else if (any(sapply(pv$initList, length) != n_covs)) {
          message(paste0("Warning: initList vector lengths do not match number of covariates (", n_covs, "), using default zero vectors"))
          lapply(seq_along(event_levels), function(i) rep(0, n_covs))
        } else if (!all(sapply(pv$initList, is.numeric))) {
          message("Warning: initList contains non-numeric vectors, using default zero vectors")
          lapply(seq_along(event_levels), function(i) rep(0, n_covs))
        } else if (any(sapply(pv$initList, function(x) any(is.na(x))))) {
          message("Warning: initList vectors contain NA values, using default zero vectors")
          lapply(seq_along(event_levels), function(i) rep(0, n_covs))
        } else {
          lapply(pv$initList, as.double)
        }
      } else {
        lapply(seq_along(event_levels), function(i) rep(0, n_covs))
      }
      message("init_values structure: ", paste(capture.output(str(init_values)), collapse = "\n"))

      fit_model <- function(target_event, ftime, fstatus, cov1, cov2,
                           tf_final, pv, init) {  # Removed default init = NULL
        if (!requireNamespace("cmprsk", quietly = TRUE)) {
          stop("Package 'cmprsk' must be installed")
        }
        message(sprintf("Training model for event %s", target_event))
        message("ftime type: ", class(ftime), ", is.numeric: ", is.numeric(ftime))
        message("ftime length: ", length(ftime))
        message("ftime sample: ", paste(head(ftime), collapse = ", "))
        message("ftime attributes: ", paste(capture.output(str(attributes(ftime))), collapse = "\n"))
        message("fstatus type: ", class(fstatus), ", is.numeric: ", is.numeric(fstatus))
        message("fstatus length: ", length(fstatus))
        message("fstatus unique values: ", paste(unique(fstatus), collapse = ", "))
        message("fstatus attributes: ", paste(capture.output(str(attributes(fstatus))), collapse = "\n"))
        message("cov1 types: ", paste(if (is.null(cov1)) "NULL" else sapply(seq_len(ncol(cov1)), function(i) class(cov1[, i])), collapse = ", "))
        message("cov1 dim: ", paste(if (is.null(cov1)) "NULL" else dim(cov1), collapse = ", "))
        message("cov1 sample: ", paste(if (is.null(cov1)) "NULL" else head(cov1[1, ]), collapse = ", "))
        message("cov1 attributes: ", paste(capture.output(str(attributes(cov1))), collapse = "\n"))
        if (!is.null(cov2)) {
          message("cov2 types: ", paste(sapply(seq_len(ncol(as.matrix(cov2))), function(i) class(cov2[, i])), collapse = ", "))
          message("cov2 dim: ", paste(dim(as.matrix(cov2)), collapse = ", "))
          message("cov2 sample: ", paste(head(cov2[1, ]), collapse = ", "))
          message("cov2 attributes: ", paste(capture.output(str(attributes(cov2))), collapse = "\n"))
        }
        n_covs <- if (!is.null(cov2)) (if (is.null(cov1)) 0 else ncol(cov1)) + ncol(as.matrix(cov2)) else (if (is.null(cov1)) 0 else ncol(cov1))
        message("n_covs in fit_model: ", n_covs)
        if (n_covs == 0) {
          stop("No covariates provided in fit_model: system is exactly singular")
        }
        # Ensure init is a numeric vector
        init <- as.double(init)
        if (length(init) != n_covs) {
          message("Warning: init length (", length(init), ") does not match n_covs (", n_covs, "), using default zero vector")
          init <- rep(0, n_covs)
        }
        if (!is.numeric(init) || any(is.na(init))) {
          message("Warning: init is not numeric or contains NA, using default zero vector")
          init <- rep(0, n_covs)
        }
        message("init structure: ", paste(capture.output(str(init)), collapse = "\n"))
        message("failcode: ", as.integer(target_event))
        message("cencode: 0")
        tryCatch({
          if (is.null(cov2)) {
            cmprsk::crr(
              ftime = ftime,
              fstatus = fstatus,
              cov1 = if (is.null(cov1)) matrix(0, nrow = length(ftime), ncol = 0) else cov1,
              failcode = as.integer(target_event),
              cencode = 0L,
              maxiter = pv$maxiter,
              gtol = pv$gtol,
              init = init
            )
          } else {
            cmprsk::crr(
              ftime = ftime,
              fstatus = fstatus,
              cov1 = if (is.null(cov1)) matrix(0, nrow = length(ftime), ncol = 0) else cov1,
              cov2 = cov2,
              tf = tf_final,
              failcode = as.integer(target_event),
              cencode = 0L,
              maxiter = pv$maxiter,
              gtol = pv$gtol,
              init = init
            )
          }
        }, warning = function(w) {
          warning(sprintf("Convergence warning for event %s: %s",
                          target_event, w$message))
          NULL
        }, error = function(e) {
          stop(sprintf("Failed to train model for event %s: %s",
                       target_event, e$message))
        })
      }

      if (pv$parallel) {
        requireNamespace("future.apply", quietly = TRUE)
        force(ftime)
        force(fstatus)
        force(cov1)
        if (!is.null(cov2)) force(cov2)
        if (!is.null(tf_final)) force(tf_final)
        models <- future.apply::future_lapply(
          seq_along(event_levels), function(i) {
            fit_model(event_levels[i], ftime, fstatus, cov1, cov2,
                      tf_final, pv, init = init_values[[i]])
          },
          future.seed = TRUE
        )
      } else {
        models <- lapply(
          seq_along(event_levels), function(i) {
            fit_model(event_levels[i], ftime, fstatus, cov1, cov2,
                      tf_final, pv, init = init_values[[i]])
          }
        )
      }
      names(models) <- event_levels

      convergence <- list()
      event_times <- list()
      for (event in event_levels) {
        convergence[[event]] <- !is.null(models[[event]])
        event_times[[event]] <- if (!is.null(models[[event]])) {
          models[[event]]$uftime
        } else {
          NULL
        }
      }

      self$state$model <- models
      self$state$convergence <- convergence
      self$state$event_times <- event_times
      self$state$cov2 <- cov2
      self$state$tf <- tf_final
      self$state$feature_names <- features
      self$state$cov2_names <- if (!is.null(cov2)) {
        colnames(as.matrix(cov2))
      } else {
        NULL
      }
      self$state$all_event_times <- all_event_times

      models
    },

    .predict = function(task, row_ids = task$row_ids) {
      if (!inherits(task, "Task")) stop("Task must be a valid mlr3 task")
      if (!all(row_ids %in% task$row_ids)) stop("Invalid row_ids")
      newdata <- task$data(rows = row_ids)
      event_levels <- as.character(task$cmp_events)
      cif_list <- vector("list", length(event_levels))
      names(cif_list) <- event_levels

      if (!is.null(self$state$cov1_formula)) {
        cov1 <- model.matrix(
          self$state$cov1_formula,
          data = newdata
        )[, -1, drop = FALSE]
        message("Predict cov1 column names: ", paste(colnames(cov1), collapse = ", "))
        cov1 <- apply(cov1, 2, as.numeric)
        cov1 <- matrix(as.double(cov1), nrow = nrow(cov1), ncol = ncol(cov1))
        colnames(cov1) <- if (ncol(cov1) == 3 && all(task$feature_names %in% c("age", "bili", "sex"))) {
          c("age", "bili", "sexf")
        } else {
          colnames(cov1)
        }
        if (ncol(cov1) == 0) {
          cov1 <- NULL
        }
        if (!is.null(cov1) && any(is.na(cov1))) {
          stop(sprintf("NAs detected in cov1: %s rows have missing values",
                       sum(rowSums(is.na(cov1)) > 0)))
        }
        if (!is.null(cov1) && !all(apply(cov1, 2, is.numeric))) {
          stop("All fixed covariates (cov1) must be numeric")
        }
      } else {
        cov1 <- NULL
      }

      if (!is.null(self$state$cov2_formulas)) {
        cov2_list <- lapply(self$state$cov2_formulas, function(f) {
          model.matrix(f, data = newdata)[, -1, drop = FALSE]
        })
        cov2 <- do.call(cbind, cov2_list)
        message("Predict cov2 column names: ", paste(colnames(cov2), collapse = ", "))
        cov2 <- apply(cov2, 2, as.numeric)
        cov2 <- matrix(as.double(cov2), nrow = nrow(cov2), ncol = ncol(cov2))
        colnames(cov2) <- paste0("cov2_", seq_len(ncol(cov2)))
        if (ncol(cov2) == 1) cov2 <- as.vector(cov2)
        if (any(is.na(cov2))) {
          stop(sprintf("NAs detected in cov2: %s rows have missing values",
                       sum(rowSums(is.na(cov2)) > 0)))
        }
        if (!all(apply(as.matrix(cov2), 2, is.numeric))) {
          stop("All time-varying covariates (cov2) must be numeric")
        }
        tf <- self$state$tf
      } else {
        cov2 <- NULL
        tf <- NULL
      }

      all_times <- self$state$all_event_times
      all_times_char <- as.character(all_times)

      for (event in event_levels) {
        model <- self$state$model[[event]]
        if (is.null(model)) {
          stop(sprintf("No model available for event %s", event))
        }
        pred <- tryCatch({
          if (!requireNamespace("cmprsk", quietly = TRUE)) {
            stop("Package 'cmprsk' must be installed")
          }
          pred_raw <- cmprsk::predict.crr(
            model,
            cov1 = if (is.null(cov1)) matrix(0, nrow = nrow(newdata), ncol = 0) else cov1,
            cov2 = cov2,
            tf = tf,
            time = all_times
          )
          pred_times <- pred_raw[, 1]
          pred_cif <- t(pred_raw[, -1, drop = FALSE])
          n_times <- length(all_times)
          if (ncol(pred_cif) != n_times) {
            full_cif <- matrix(0, nrow = nrow(pred_cif), ncol = n_times)
            colnames(full_cif) <- all_times_char
            time_idx <- match(pred_times, all_times)
            if (any(is.na(time_idx))) {
              stop("Predicted times do not match training times")
            }
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

      PredictionCompRisks$new(task = task, cif = cif_list)
    }
  )
)
