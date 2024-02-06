#' RMSE
#' @name RMSE
#' @description calculates root mean square error (RMSE) for model testing
#'
#' @param observed vector of observed values
#' @param predicted vector of predicted values
#' @return numeric vector
#' @export
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}


