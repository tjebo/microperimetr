#' predict_norm
#' @name predict_norm
#' @author tjebo
#' @description location based prediction of retinal sensitivity
#' using a linear model for each location of the grid underlying [norm_data]
#' with age, sex and lens status
#' as covariates. The output of this function is used for the interpolation
#' with [interpolate_norm]
#' @param testdata if passed, the testtype of the test will be extracted
#' @param age age of individual to be predicted. defaults to 50.
#' @param sex sex of individual to be predicted. defaults to 'm'.
#' @param lens lens status. must be either 'natural' (default) or 'pseudo'.
#'   Defaults to 'natural'
#' @param interval used in \link[stats]{predict.lm}
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @family prediction functions
#' @examples
#' preddat <- predict_norm(testdat1)
#' testdat_coord <- coord_cart(testdat1)
#' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' @return Data frame with predicted sensitivity values for all test types
#'   at each location of the grid underlying [norm_data]
#' @export

predict_norm <- function(testdata = NULL,
                         age = NULL, sex = NULL, lens = NULL,
                         interval = "prediction") {

  if (!is.null(testdata)) {
    if(length(unique(testdata$testID)) > 1){
      stop('Too many tests. Create a list of unique tests.', call. = FALSE)
    }
    age <- as.numeric(unique(testdata$age))
    sex <- unique(testdata$sex)
    lens <- unique(testdata$lens)
  }

  if (is.null(age)) {
    warning("age is not set - default to 50 years")
    age <- 50L
  }
  if (is.null(sex)) {
    warning("sex is not set - default to 'm'")
    sex <- "m"
  }
  if (is.null(lens)) {
    warning("lens is not set - default to 'natural'")
    lens <- "natural"
  }

  if (!lens %in% c("natural", "pseudo")) {
    stop("lens needs to be either 'natural' or 'pseudo')")
  }
  if (!sex %in% c("m", "f")) stop("sex needs to be either 'm' or 'f')")

  testtype_unq <- unique(testdata$testtype)

  data_model <-
    filter(microperimetr::norm_data, .data$testtype == testtype_unq) %>%
    select(-'testID') %>%
    mutate(testnumber = paste0("E", .data$testnumber)) %>%
    pivot_wider(names_from = "testnumber", values_from = "value") %>%
    mutate(
      MeanSens = (.data$E1 + .data$E2) / 2,
      position = paste(.data$eccent, .data$angle, sep = "_")
    ) %>%
    select(-"E1", -"E2")

  list_point <-
    split(data_model, data_model$position)

  list_lm <- lapply(
    list_point,
    function(x) stats::lm(MeanSens ~ age + lens + sex, data = x)
  )

  newdata <- data.frame(age = age, sex = sex, lens = lens)

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
#' preddat <- predict_norm(testdat1)
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
#' preddat <- predict_norm(testdat1)
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
  interpol_dat <- select(pred_data_coord, "x", "y", "fit", "lwr")
  sp::coordinates(interpol_dat) <- ~ x + y

  # fit with inverted distance weighting
  # vgm fitting with set of models, returning the better fitting model
  # currently creates warnings because likely the variogram model used
  # is not a good choice!!
  # fit (mean sensitivity)
  gstat_fit <- gstat::gstat(formula = fit ~ 1, data = interpol_dat)
  vario_fit <- gstat::variogram(gstat_fit)
  fit_fit <-suppressWarnings(
    gstat::fit.variogram(vario_fit, gstat::vgm(c("Exp", "Sph", "Mat")))
  )
  fit <- gstat::krige(fit ~ 1, interpol_dat, sp_grid, fit_fit)
  # lwr (lower value of error interval)
  gstat_lwr <- gstat::gstat(formula = lwr ~ 1, data = interpol_dat)
  vario_lwr <- gstat::variogram(gstat_lwr)
  fit_lwr <- suppressWarnings(
    gstat::fit.variogram(vario_lwr, gstat::vgm(c("Exp", "Sph", "Mat")))
  )
  lwr <- gstat::krige(lwr ~ 1, interpol_dat, sp_grid, fit_lwr)

  warning(
  'Warnings were suppressed that indicate inadequate use of variogram models',
  call. = FALSE
  )
  interpol_res <- cbind(dense_grid,
    fit = fit$var1.pred,
    lwr = lwr$var1.pred,
    upr = fit$var1.pred + fit$var1.pred - lwr$var1.pred
  )
  interpol_res
}

#' compare
#' @name compare
#' @description Interpolates normal values and compares with test values.
#' This is basically a wrapper around [predict_norm], [coord_cart] and
#'   [interpolate_norm]
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
    preddat <- predict_norm(test_list_elem)
    testdat_coord <- coord_cart(test_list_elem)
    interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
  }

  interpol_list <- lapply(test_list, run_wrapper)
class(interpol_list) <- c("compare","list")
interpol_list
}
