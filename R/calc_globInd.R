#' calc_globInd
#'
#' @author tjebo
#'
#' @description calculates global indices (mean deviation and pattern standard deviation) from given vectors
#'
#' @param df Dataframe in which the vectors are found
#' @param test_val observed sensitivities (vector)
#' @param norm_val normal sensitivities for the same points
#' @param var_norm variance of normal values for the same points
#'
#' @return named vector with mean deviation (MDev) and pattern standard deviation (PSD)
#' @export


calc_globInd <- function (df, test_val, norm_val, var_norm) {

  test_val_sub <- deparse(substitute(test_val))
  norm_val_sub <- deparse(substitute(norm_val))
  var_norm_sub <- deparse(substitute(var_norm))

  if(test_val_sub %in% names(df) == TRUE) {
    tv <- df[[test_val_sub]]
  } else if (is.atomic(test_val) == TRUE) {
    tv <- test_val
  }
  if(norm_val_sub %in% names(df) == TRUE) {
    nv <- df[[norm_val_sub]]
  } else if (is.atomic(norm_val) == TRUE) {
    nv <- norm_val
  }

  if(var_norm_sub %in% names(df) == TRUE) {
    vn <- df[[var_norm_sub]]
  } else if (is.atomic(var_norm) == TRUE) {
    vn <- var_norm
  }


  MDev <- mean((tv - nv)/ vn, na.rm = T)/
    mean(1/vn, na.rm = T)

  PSD <- sqrt(mean(vn, na.rm = T) *
                (sum((tv-nv-MDev)^2/vn)/(length(vn)-1)))

  result <- c(MeanDev = MDev, PSD = PSD)
  result
}
