# File: R/zzz.R

#' @importFrom mlr3 mlr_learners
.onLoad <- function(libname, pkgname) { # nolint
  # Register the learner with mlr3's learners dictionary
  mlr_learners$add("cmprisk.crr", LearnerCompRisksFineGrayCRR)
}