#' Calculates microperimetry statistics.
#' @name mpstats
#' @description Calculates microperimetry statistics from a test or list of tests.
#'   This is basically a wrapper around [predict_norm], [coord_cart],
#'   [interpolate_norm] and [mpstats_single]
#'   Includes mean sensitivity (mean_sens), mean deviation (mean_dev),
#'   pattern standard deviation (psd).
#'   You can also pass a compare object directly (from compare function),
#'   thus saving time for duplicate kringing.
#' @details **mean_dev** and **psd** are estimated.
#'   This is, because there is no 'real mean' and, more importantly,
#'   no "real variance" of normal values for each given location.
#'   Mean and variance are estimated based on kringing (spatial interpolation)
#'   of *predicted (!)* values. The predicted values are given by simple linear
#'   regression for each location, based on the [norm_data].
#'   It includes age, sex and lens status as covariates.
#'   The variance used for estimation is equal to the *prediction intervals*
#'   from those models, assuming homoscedasticity in a non-weighted model
#' @seealso [stats::predict.lm], [predict_norm], [coord_cart],
#'   [interpolate_norm]
#' @param testdata microperimetry test data
#' @param digits Digits to be rounded to
#' @family stat functions
#' @return Matrix
#' @examples
#' test_compare <- compare(testdata)
#' mpstats(test_compare)
#' # or (same result)
#' mpstats(testdata)
#' @author tjebo
#' @export

mpstats <- function(testdata, digits = 2) {

  if(inherits(testdata, 'list')){

    vec_l <- lengths(lapply(testdata, function(x) unique(paste(x$testID, x$testtype))))
    if(!all(vec_l == 1)){
      stop('Too many tests. Create a list of unique tests.', call. = FALSE)
    }
    test_list <- testdata
  } else {
    testdata$test_unq <- paste(testdata$testID, testdata$testtype, sep = '_')
    test_list <- split(testdata, testdata$test_unq)
  }
#return(test_list)
  if(!inherits(testdata, "compare")){
  run_wrapper_full <- function(test_list_elem){
  preddat <- predict_norm(test_list_elem)
  testdat_coord <- coord_cart(test_list_elem)
  interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
  mpstats <- mpstats_single(interpol_dat)
  }
  mpstats_list <- lapply(test_list, run_wrapper_full)
  } else {
    mpstats_list <- lapply(test_list, mpstats_single)
  }
  mpstats <- do.call(rbind, mpstats_list)
  rownames(mpstats) <- names(mpstats_list)
  mpstats
}


#' Microperimetry statistics.
#' @name mpstats_single
#' @description Calculates microperimetry statistics from a single test.
#'   Includes mean sensitivity (mean_sens), mean deviation (mean_dev),
#'   pattern standard deviation (psd). See also details.
#'   Works in conjunction with prediction functions. (see example)
#' @details **mean_dev** and **psd** are estimated.
#'   This is, because there is no 'real mean' and, more importantly,
#'   no "real variance" of normal values for each given location.
#'   Mean and variance are estimated based on kringing (spatial interpolation)
#'   of *predicted (!)* values. The predicted values are given by simple linear
#'   regression for each location, based on the [norm_data].
#'   It includes age, sex and lens status as covariates.
#'   The variance used for estimation is equal to the *prediction intervals*
#'   from those models, assuming homoscedasticity in a non-weighted model
#' @seealso [stats::predict.lm]
#' @param interpol_dat interpolated results from test data.
#' @param digits Digits to be rounded to
#' @family stat functions
#' @return vector
#' @examples
#' preddat <- predict_norm(testdat1)
#' testdat_coord <- coord_cart(testdat1)
#' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' mpstats_single(interpol_dat)
#' @author tjebo
#' @export

mpstats_single <- function(interpol_dat, digits = 2) {
  # remove blind spot. Make sure this remains stimID for blind spot in maia import!!!
  data <- interpol_dat[interpol_dat$stimID != 0,]
  if (is.null(interpol_dat$fit) | is.null(interpol_dat$lwr) | is.null(interpol_dat$upr)) {
    stop('Data should contain columns \"fit\", \"lwr\" and \"upr\"
       Ideally, pass the result of interpolate_norm.')
  }
  interpol_dat$pred_int <- interpol_dat$upr - interpol_dat$lwr

  testval <- interpol_dat$value
  normval <- interpol_dat$fit
  normvar <- interpol_dat$pred_int

  mean_sens <- mean(testval, na.rm = TRUE)
  sd_sens <- stats::sd(testval, na.rm = TRUE)
  mean_dev <- mean((testval - normval) / normvar, na.rm = TRUE) /
    mean(1 / normvar, na.rm = TRUE)

  psd <- sqrt(mean(normvar, na.rm = TRUE) *
    (sum((testval - normval - mean_dev)^2 / normvar, na.rm = TRUE) /
       (length(normvar) - 1)))

  mpstats <-
    round(
      cbind( mean_sens = mean_sens,
             sd_sens = sd_sens,
             mean_dev = mean_dev,
             psd = psd),
      digits = digits
    )
  mpstats
}

#' "Manual" calculation of microperimetry statistics
#' @name mpstats_manual
#' @author tjebo
#' @description
#' Calculates mean deviation (MD) and pattern standard deviation (psd)
#'   manually from data or vectors.
#' @param test_val observed sensitivities. Can be column name (quoted or unquoted) or numeric vector
#' @param norm_val mean of normal sensitivities of same length as test_val. Can be column name (quoted or unquoted) or numeric vector
#' @param var_norm variance of normal values of same length as test_val. Can be column name (quoted or unquoted) or numeric vector
#' @param data **optional** Dataframe with vectors for analysis
#' @family stat functions
#' @return named vector with mean deviation (mean_dev) and pattern standard deviation (psd)
#' @export
mpstats_manual <- function(test_val, norm_val, var_norm, data = NULL) {
  test_val_sub <- deparse(substitute(test_val))
  norm_val_sub <- deparse(substitute(norm_val))
  var_norm_sub <- deparse(substitute(var_norm))

  if (test_val_sub %in% names(data)) {
    testval <- data[[test_val_sub]]
  } else if (is.atomic(test_val)) {
    testval <- test_val
  }
  if (norm_val_sub %in% names(data)) {
    normval <- data[[norm_val_sub]]
  } else if (is.atomic(norm_val)) {
    normval <- norm_val
  }

  if (var_norm_sub %in% names(data)) {
    normvar <- data[[var_norm_sub]]
  } else if (is.atomic(var_norm)) {
    normvar <- var_norm
  }
  if (length(normval) != length(testval)) {
    stop("norm_val needs to be of same length as test_val")
  }
  if (length(normvar) != length(testval)) {
    stop("norm_var needs to be of same length as test_val")
}
  mean_dev <- mean((testval - normval) / normvar, na.rm = TRUE) /
    mean(1 / normvar, na.rm = TRUE)

  psd <- sqrt(mean(normvar, na.rm = TRUE) *
    (sum((testval - normval - mean_dev)^2 / normvar, na.rm = TRUE) /
       (length(normvar) - 1)))
}
