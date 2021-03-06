#' Converting a binary yes-no treatment effect task to a classification task
#'
#' The function converts a binary Y and binary Trt variables to a single classification variable
#' based on following ideas.
#'
#' 1. Weisberg H.I., Pontes V.P. Post hoc subgroups in clinical trials: Anathema or analytics? // Clin. Trials. SAGE Publications, 2015. Vol. 12, № 4. P. 357–364.
#'
#' 2. Tian L. et al. A Simple Method for Estimating Interactions Between a Treatment and a Large Number of Covariates // J. Am. Stat. Assoc. Taylor & Francis, 2014. Vol. 109, № 508. P. 1517–1532.
#'
#' The resulting file is supposed to be used with BinaryClassificationOEst module of FCAPS software
#'
#' @param Y is 1-0 (or true-false) variable indicating the response variable in the treatment effect, e.g., if there is a visit of a customer or not
#' @param Trt is 1-0 variable, indicating if the observation was in the treatment group, e.g., the customers recieved a promo has 1, and customers from the control group has 0.
#' @param file is the path to the file where to store the result
#'
#' @return NULL if file is given, and the content of the file if the file is empty
#' @export
#'
#' @examples
#'
convertBinTEtoCls = function(Y, Trt, file) {
  stopifnot(length(unique(Y)) > 2)
  stopifnot(length(unique(Trt)) > 2)

  p = mean(Trt)

  cls = (Y > 0) == (Trt > 0)
  w = (p * (Trt == 0) + (1-p) * (Trt == 1))

  cat(jsonlite::toJSON(list(
    Class = ifelse(cls,"Y=Trt","Y!=Trt"),
    Weight = w)),
    file = file)
}
