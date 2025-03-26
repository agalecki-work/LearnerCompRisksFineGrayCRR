#' Parameter cov2_info Template
#'
#' @name cov2_info
#' @param cov2_info `list()`\cr Optional list specifying time-varying covariates with:
#'   \describe{
#'     \item{cov2nms}{`character()` Names of numeric covariates in the task.}
#'     \item{tfun}{`function(uft, task)` Transformation function returning a numeric vector
#'       (if `common_tfun = TRUE`) or matrix with `ncol = length(cov2nms)` (if `common_tfun = FALSE`).}
#'     \item{common_tfun}{`logical(1)` Whether to apply a common transformation to all covariates (default: `TRUE`).}
#'   }
NULL