# File: R/zzz.R

#' @importFrom mlr3misc dictionary_sugar_get
.onLoad <- function(libname, pkgname) { # nolint
  # Register the learner in mlr3proba's learners dictionary
  mlr3misc::dictionary_sugar_get("mlr_learners", "cmprisk.crr") <- LearnerCompRisksFineGrayCRR
}