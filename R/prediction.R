#' linear model for each location
#' @name lm_loc
#' @author tjebo
#' @description linear regression model for each test location
#' of the grid underlying [norm_data], with age as predictor
#'  The output of this function is used for the prediction in
#' [predict_norm]
#' @param testdata if given, the lm will only be created for testtype
#'   of the testdata
#' @param normdata data used for prediction, defaults to norm_data.
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @family prediction functions
#' @examples
#' lm_dat <- lm_loc(testdata = testdat1)
#' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' testdat_coord <- coord_cart(testdat1)
#' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' @return List
#' @details Returns list of linear models for all test types
#'   at each location of the grid underlying [norm_data]
#' @export
lm_loc <- function(testdata = NULL, normdata = NULL){

  if(is.null(normdata)) normdata <- microperimetr::data_model

if(!is.null(testdata)) {
  testtype_unq <- unique(testdata$testtype)[, drop = TRUE]
  normdata <- normdata[normdata$testtype %in% testtype_unq,]
}
list_norm <- split(normdata, normdata$testtype)

lm_testtype <- function(x){

list_point <-
  split(x, x$position)

list_lm <- lapply(
  list_point,
  function(x) stats::lm(MeanSens ~ age, data = x)
)
}
list_lm_testtype <- lapply(list_norm, lm_testtype)
list_lm_testtype
}

#' Predict sensitivity location-wise
#' @name predict_norm
#' @author tjebo
#' @description location based prediction of retinal sensitivity. Linear model
#' from [loc_lm]. Output for [interpolate_norm]
#' @param testdata if passed, the testtype of the test will be extracted
#' @param age age of individual to be predicted. defaults to 50.
#' @param interval used in \link[stats]{predict.lm}
#' @param normdata data used for prediction, defaults to norm_data.
#' @param list_model **required** list of models for each location
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @family prediction functions
#' @examples
#' lm_dat <- lm_loc(testdata = testdat1)
#' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' testdat_coord <- coord_cart(testdat1)
#' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' @return Data frame with predicted sensitivity values for all test types
#'   at each location of the grid underlying [norm_data]
#' @export

predict_norm <- function(testdata = NULL,
                         list_model,
                         age = NULL,
                         interval = "prediction") {
  if (!is.null(testdata)) {
    if(length(unique(testdata$testID)) > 1){
      stop('Too many tests. Create a list of unique tests.', call. = FALSE)
    }
    age <- as.numeric(unique(testdata$age))
    # sex <- unique(testdata$sex)
    # lens <- unique(testdata$lens)
  }

  if (is.null(age)) {
    warning("age is not set - default to 50 years", call. = FALSE)
    age <- 50L
  }
  # if (is.null(sex)) {
  #   warning("sex is not set - default to 'm'", call. = FALSE)
  #   sex <- "m"
  # }
  # if (is.null(lens)) {
  #   warning("lens is not set - default to 'natural'", call. = FALSE)
  #   lens <- "natural"
  # }

  # if (!lens %in% c("natural", "pseudo")) {
  #   stop("lens needs to be either 'natural' or 'pseudo')")
  # }
  # if (!sex %in% c("m", "f")) stop("sex needs to be either 'm' or 'f')")
if(!is.null(testdata)) {
  testtype_unq <- unique(testdata$testtype)
  list_lm <- list_model[[testtype_unq]]
} else{list_lm <- list_model
}


  newdata <- data.frame(age = age)

  res_pred <- lapply(
      list_lm,
      function(x) stats::predict(x, newdata = newdata, interval = interval)
      )

  res_pred <- do.call(rbind, lapply(res_pred, unlist))

  rownames(res_pred) <- names(list_lm)

  results <- res_pred %>%
    as.data.frame() %>%
    tibble::rownames_to_column("eccent_angle") %>%
    separate(.data$eccent_angle, c("eccent", "angle")) %>%
    mutate_all("as.numeric")

  results
}

#' coord_cart
#' @name coord_cart
#' @description Adding cartesian coordinates to test results
#' @param testdata **required**. Data frame, output from [read_maia].
#' @return Data frames
#' @examples
#' lm_dat <- lm_loc(testdata = testdat1)
#' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' testdat_coord <- coord_cart(testdat1)
#' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' @family prediction functions
#' @export
coord_cart <- function(testdata) {
  testdat_coord <- testdata %>%
  mutate(
    x = round(cos(.data$angle * pi / 180) * as.numeric(.data$eccent), 3),
    y = round(sin(.data$angle * pi / 180) * as.numeric(.data$eccent), 3)
  )
testdat_coord
}
#'

#' interpolate_norm
#' @name interpolate_norm
#' @description Interpolates grid based on local predictions of normal data.
#' @author tjebo and max
#' @param pred_data predicted norm data from [predict_norm]
#' @param newgrid custom grid to be interpolated on. If not specified, regular grid will be interpolated with resolution given in grid_density
#' @param grid_density resolution of interpolated grid in degress.
#' @import gstat
#' @import sp
#' @return List of data frames with predicted results for grid for all test types and plot if graph = TRUE
#' @family prediction functions
#' @examples
#' # Tests were performed with leave-one out cross validation
#' #LOOCV of data points from interpol_dat data
#' #x <- gstat::krige.cv(fit ~ 1, interpol_dat, interpol_dat,
#' #       model = fit_fit, nfold = nrow(interpol_dat))
#' #RMSE(x$var1.pred,x$observed)
#'
#' lm_dat <- lm_loc(testdata = testdat1)
#' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' testdat_coord <- coord_cart(testdat1)
#' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' @export
#'
interpolate_norm <- function(pred_data,
                             newgrid = NULL,
                             grid_density = 0.2) {
  # create grid
  if (!is.null(newgrid)) {
    dense_grid <- newgrid
    sp_grid <- dense_grid
    sp::coordinates(sp_grid) <- ~ x + y
  } else if (is.null(newgrid)) {
    dense_grid <- expand.grid(x = seq(-10, 10, by = grid_density), y = seq(-10, 10, by = grid_density))
    sp_grid <- dense_grid
    sp::coordinates(sp_grid) <- ~ x + y
    sp::gridded(sp_grid) <- TRUE
  }

  pred_data_coord <- coord_cart(pred_data)

  interpol_dat <- pred_data_coord
  sp::coordinates(interpol_dat) <- ~ x + y

  # fit with inverted distance weighting
  # vgm fitting with set of models, returning the better fitting model
  # currently creates warnings because likely the variogram model used
  # is not a good choice!!
 if('fit' %in% names(interpol_dat)){
  # fit (mean sensitivity)
  gstat_fit <- gstat::gstat(formula = fit ~ 1, data = interpol_dat)
  vario_fit <- gstat::variogram(gstat_fit)
  fit_fit <-suppressWarnings(
    gstat::fit.variogram(vario_fit, gstat::vgm(c("Exp", "Sph", "Mat")))
  )
  fit <- gstat::krige(fit ~ 1, interpol_dat, sp_grid, fit_fit)
 } else if ('value' %in% names(interpol_dat)){
   # fit (mean sensitivity)
   gstat_fit <- gstat::gstat(formula = value ~ 1, data = interpol_dat)
   vario_fit <- gstat::variogram(gstat_fit)
   fit_fit <-suppressWarnings(
     gstat::fit.variogram(vario_fit, gstat::vgm(c("Exp", "Sph", "Mat")))
   )
   fit <- gstat::krige(value ~ 1, interpol_dat, sp_grid, fit_fit)
 }
  if ('lwr' %in% names(interpol_dat)){
  # lwr (lower value of error interval)
  gstat_lwr <- gstat::gstat(formula = lwr ~ 1, data = interpol_dat)
  vario_lwr <- gstat::variogram(gstat_lwr)
  fit_lwr <- suppressWarnings(
    gstat::fit.variogram(vario_lwr, gstat::vgm(c("Exp", "Sph", "Mat")))
  )
  lwr <- gstat::krige(lwr ~ 1, interpol_dat, sp_grid, fit_lwr)
}
  warning(
  'Warnings were suppressed that indicate inadequate use of variogram models',
  call. = FALSE
  )
  interpol_res <- cbind(dense_grid, fit = fit$var1.pred)
  if ('fit' %in% names(interpol_dat)){
    interpol_res <-
      cbind(interpol_res,
            lwr = lwr$var1.pred,
            upr = fit$var1.pred + fit$var1.pred - lwr$var1.pred
  )
  }
  interpol_res
}

#' variation of the visual field
#' @name field_variation
#' @author tjebo
#' @description generating field variations based on simple kriging
#'   of normal fields, without the step of predicting location with
#'  linear models first.
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @family prediction functions
#' @examples
#' field_var <- field_variation()
#' bebie_stats <- calc_bebie(testdat1, field_var)
#' @return data frame
#' @export
field_variation <- function() {
  # testtype_unq <- unique(testdata$testtype)
  # if (length(testtype_unq) > 1) {
  #   stop("Too many tests. Create a list of unique tests.", call. = FALSE)
  # }
  data_split <- data_model %>%
    dplyr::filter(testtype != 'cr_diff') %>%
    mutate(value = MeanSens) %>%
    drop_na(value) %>%
    split(., interaction(.$testtype,.$patID))

  interpolate_field <- function(x) {
    inter_test <- interpolate_norm(pred_data = x, grid_density = 0.25) %>%
      mutate_at(.vars = vars(x, y), .funs = round, digits = 2)
    inter_test
  }
  res_fields <- lapply(data_split, interpolate_field)

  res_fields_bind <- bind_rows(res_fields, .id = "normID") %>%
    mutate(fit = round(fit, 2))
  res_fields_bind
  class(res_fields_bind) <- c("field_var","tbl_df", "tbl", "data.frame")
  res_fields_bind
}

#' compare
#' @name compare
#' @description Interpolates normal values and compares with test values.
#' This is basically a wrapper around [lm_loc], [predict_norm],
#'   [coord_cart] and [interpolate_norm]
#' @param testdata **required**. Data frame with maia test results. See details
#' @details **testdata**: This should be ideally output from [read_maia].
#' The data frame must contain following columns:
#' **patID, eye, testID, testtype, eccent, angle, value, stimID**
#'
#' @import dplyr
#' @author tjebo
#' @return data frames with original and predicted normal values.
#' @export
compare <- function(testdata) {

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
  run_wrapper <- function(test_list_elem){
    list_lm <- lm_loc(test_list_elem)
    preddat <- predict_norm(test_list_elem, list_model = list_lm)
    testdat_coord <- coord_cart(test_list_elem)
    interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
  }

  interpol_list <- lapply(test_list, run_wrapper)
class(interpol_list) <- c("compare","list")
interpol_list
}
