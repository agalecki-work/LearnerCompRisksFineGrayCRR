mlr3extralearners::create_learner(
  path = ".",
  classname = "FineGrayCRR",
  type = "regr",
  key = "finegraycrr",
  algorithm = "F-G Competing Risks",
  package = "cmprisk",
  caller  = "crr",
  feature_types = c("logical", "integer", "numeric", "factor"),
  predict_types = "response",
  properties = c("importance", "missings"),
  gh_name = "agalecki",
  label = "Fine-Gray Competing Risks Regression"
)
