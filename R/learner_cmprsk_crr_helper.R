#'
#' A helper function to skip a test in `testthat` if a specified R package is not installed.
#' It checks if the package is available using `requireNamespace()` and skips the test with
#' an informative message if the package is not found.
#'
#' @param pkg `character(1)` \cr
#'   The name of the package to check for installation.
#' @return Invisible `NULL`. The function is called for its side effect of skipping the test.
#' @keywords internal
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste("Package", pkg, "not installed"))
  }
}


#' Configure a task for competing risks analysis
#' @param features Character vector of feature names.
#' @param stratum Character name of the stratum column.
#' @return An mlr3 task.
#' @keywords internal
# Task configuration function
configure_task <- function(task_name = "pbc", stratum = NULL, features = "trt") {
  task <- mlr3::tsk(task_name)
  data_cols <- names(task$data())
  if (!is.null(stratum) && !stratum %in% data_cols) {
    stop(sprintf("Stratum variable '%s' not found in task", stratum))
  }
  if (!is.null(features) && !all(features %in% data_cols)) {
    stop(sprintf("Features %s not found in task",
                 paste(setdiff(features, data_cols), collapse = ", ")))
  }
  if (!is.null(features)) {
    task$select(features)
  }
  if (!is.null(stratum)) {
    task$set_col_roles(cols = stratum, add_to = "stratum")
  }
  task
}



# Partition function
#' Create a train-test partition for a task
#' @param task An mlr3 task.
#' @return A list with train and test row IDs.
#' @keywords internal
create_partition <- function(task, ratio = 0.7) {
  set.seed(123)
  mlr3::partition(task, ratio = ratio)
}
