#' ## for perimeter package
#' #' linear model for each location
#' #' @name lm_loc
#' #' @author tjebo
#' #' @description linear regression model for each test location
#' #' of the grid underlying [norm_data], with age as predictor
#' #'  The output of this function is used for the prediction in
#' #' [predict_norm]
#' #' @param testdata if given, the lm will only be created for testtype
#' #'   of the testdata
#' #' @param normdata data used for prediction, defaults to norm_data.
#' #' @import dplyr
#' #' @import tidyr
#' #' @importFrom rlang .data
#' #' @family prediction functions
#' #' @examples
#' #' lm_dat <- lm_loc(testdata = testdat1)
#' #' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' #' testdat_coord <- coord_cart(testdat1)
#' #' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' #' @return List
#' #' @details Returns list of linear models for all test types
#' #'   at each location of the grid underlying [norm_data]
#' #' @export
#' lm_loc <- function(testdata = NULL, normdata = NULL){
#'
#'   if(is.null(normdata)) normdata <- microperimetr::data_model
#'
#'   if(!is.null(testdata)) {
#'     testtype_unq <- unique(testdata$testtype)[, drop = TRUE]
#'     normdata <- normdata[normdata$testtype %in% testtype_unq,]
#'   }
#'   list_norm <- split(normdata, normdata$testtype)
#'
#'   lm_testtype <- function(x){
#'
#'     list_point <-
#'       split(x, x$position)
#'
#'     list_lm <- lapply(
#'       list_point,
#'       function(x) stats::lm(MeanSens ~ age, data = x)
#'     )
#'   }
#'   list_lm_testtype <- lapply(list_norm, lm_testtype)
#'   list_lm_testtype
#' }
#'
#' #' Predict sensitivity location-wise
#' #' @name predict_norm
#' #' @author tjebo
#' #' @description location based prediction of retinal sensitivity. Linear model
#' #' from [loc_lm]. Output for [interpolate_norm]
#' #' @param testdata if passed, the testtype of the test will be extracted
#' #' @param age age of individual to be predicted. defaults to 50.
#' #' @param interval used in \link[stats]{predict.lm}
#' #' @param normdata data used for prediction, defaults to norm_data.
#' #' @param list_model **required** list of models for each location
#' #' @import dplyr
#' #' @import tidyr
#' #' @importFrom rlang .data
#' #' @family prediction functions
#' #' @examples
#' #' lm_dat <- lm_loc(testdata = testdat1)
#' #' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' #' testdat_coord <- coord_cart(testdat1)
#' #' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' #' @return Data frame with predicted sensitivity values for all test types
#' #'   at each location of the grid underlying [norm_data]
#' #' @export
#'
#' predict_norm <- function(testdata = NULL,
#'                          list_model,
#'                          age = NULL,
#'                          interval = "prediction") {
#'   if (!is.null(testdata)) {
#'     if(length(unique(testdata$testID)) > 1){
#'       stop('Too many tests. Create a list of unique tests.', call. = FALSE)
#'     }
#'     age <- as.numeric(unique(testdata$age))
#'     # sex <- unique(testdata$sex)
#'     # lens <- unique(testdata$lens)
#'   }
#'
#'   if (is.null(age)) {
#'     warning("age is not set - default to 50 years", call. = FALSE)
#'     age <- 50L
#'   }
#'   # if (is.null(sex)) {
#'   #   warning("sex is not set - default to 'm'", call. = FALSE)
#'   #   sex <- "m"
#'   # }
#'   # if (is.null(lens)) {
#'   #   warning("lens is not set - default to 'natural'", call. = FALSE)
#'   #   lens <- "natural"
#'   # }
#'
#'   # if (!lens %in% c("natural", "pseudo")) {
#'   #   stop("lens needs to be either 'natural' or 'pseudo')")
#'   # }
#'   # if (!sex %in% c("m", "f")) stop("sex needs to be either 'm' or 'f')")
#'   if(!is.null(testdata)) {
#'     testtype_unq <- unique(testdata$testtype)
#'     list_lm <- list_model[[testtype_unq]]
#'   } else{list_lm <- list_model
#'   }
#'
#'
#'   newdata <- data.frame(age = age)
#'
#'   res_pred <- lapply(
#'     list_lm,
#'     function(x) stats::predict(x, newdata = newdata, interval = interval)
#'   )
#'
#'   res_pred <- do.call(rbind, lapply(res_pred, unlist))
#'
#'   rownames(res_pred) <- names(list_lm)
#'
#'   results <- res_pred %>%
#'     as.data.frame() %>%
#'     tibble::rownames_to_column("eccent_angle") %>%
#'     separate(.data$eccent_angle, c("eccent", "angle")) %>%
#'     mutate_all("as.numeric")
#'
#'   results
#' }
#'
#'
#' #' interpolate_norm
#' #' @name interpolate_norm
#' #' @description Interpolates grid based on local predictions of normal data.
#' #' @author tjebo and max
#' #' @param pred_data predicted norm data from [predict_norm]
#' #' @param newgrid custom grid to be interpolated on. If not specified, regular grid will be interpolated with resolution given in grid_density
#' #' @param grid_density resolution of interpolated grid in degress.
#' #' @import gstat
#' #' @import sp
#' #' @return List of data frames with predicted results for grid for all test types and plot if graph = TRUE
#' #' @family prediction functions
#' #' @examples
#' #' # Tests were performed with leave-one out cross validation
#' #' #LOOCV of data points from interpol_dat data
#' #' #x <- gstat::krige.cv(fit ~ 1, interpol_dat, interpol_dat,
#' #' #       model = fit_fit, nfold = nrow(interpol_dat))
#' #' #RMSE(x$var1.pred,x$observed)
#' #'
#' #' lm_dat <- lm_loc(testdata = testdat1)
#' #' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' #' testdat_coord <- coord_cart(testdat1)
#' #' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' #' @export
#' #'
#' interpolate_norm <- function(pred_data,
#'                              newgrid = NULL,
#'                              grid_density = 0.2) {
#'   # create grid
#'   if (!is.null(newgrid)) {
#'     dense_grid <- newgrid
#'     sp_grid <- dense_grid
#'     sp::coordinates(sp_grid) <- ~ x + y
#'   } else if (is.null(newgrid)) {
#'     dense_grid <- expand.grid(x = seq(-10, 10, by = grid_density), y = seq(-10, 10, by = grid_density))
#'     sp_grid <- dense_grid
#'     sp::coordinates(sp_grid) <- ~ x + y
#'     sp::gridded(sp_grid) <- TRUE
#'   }
#'
#'   pred_data_coord <- coord_cart(pred_data)
#'
#'   interpol_dat <- pred_data_coord
#'   sp::coordinates(interpol_dat) <- ~ x + y
#'
#'   # fit with inverted distance weighting
#'   # vgm fitting with set of models, returning the better fitting model
#'   # currently creates warnings because likely the variogram model used
#'   # is not a good choice!!
#'   if('fit' %in% names(interpol_dat)){
#'     # fit (mean sensitivity)
#'     gstat_fit <- gstat::gstat(formula = fit ~ 1, data = interpol_dat)
#'     vario_fit <- gstat::variogram(gstat_fit)
#'     fit_fit <-suppressWarnings(
#'       gstat::fit.variogram(vario_fit, gstat::vgm(c("Exp", "Sph", "Mat")))
#'     )
#'     fit <- gstat::krige(fit ~ 1, interpol_dat, sp_grid, fit_fit)
#'   } else if ('value' %in% names(interpol_dat)){
#'     # fit (mean sensitivity)
#'     gstat_fit <- gstat::gstat(formula = value ~ 1, data = interpol_dat)
#'     vario_fit <- gstat::variogram(gstat_fit)
#'     fit_fit <-suppressWarnings(
#'       gstat::fit.variogram(vario_fit, gstat::vgm(c("Exp", "Sph", "Mat")))
#'     )
#'     fit <- gstat::krige(value ~ 1, interpol_dat, sp_grid, fit_fit)
#'   }
#'   if ('lwr' %in% names(interpol_dat)){
#'     # lwr (lower value of error interval)
#'     gstat_lwr <- gstat::gstat(formula = lwr ~ 1, data = interpol_dat)
#'     vario_lwr <- gstat::variogram(gstat_lwr)
#'     fit_lwr <- suppressWarnings(
#'       gstat::fit.variogram(vario_lwr, gstat::vgm(c("Exp", "Sph", "Mat")))
#'     )
#'     lwr <- gstat::krige(lwr ~ 1, interpol_dat, sp_grid, fit_lwr)
#'   }
#'   warning(
#'     'Warnings were suppressed that indicate inadequate use of variogram models',
#'     call. = FALSE
#'   )
#'   interpol_res <- cbind(dense_grid, fit = fit$var1.pred)
#'   if ('fit' %in% names(interpol_dat)){
#'     interpol_res <-
#'       cbind(interpol_res,
#'             lwr = lwr$var1.pred,
#'             upr = fit$var1.pred + fit$var1.pred - lwr$var1.pred
#'       )
#'   }
#'   interpol_res
#' }
#'
#' #' variation of the visual field
#' #' @name field_variation
#' #' @author tjebo
#' #' @description generating field variations based on simple kriging
#' #'   of normal fields, without the step of predicting location with
#' #'  linear models first.
#' #' @import dplyr
#' #' @import tidyr
#' #' @importFrom rlang .data
#' #' @family prediction functions
#' #' @examples
#' #' field_var <- field_variation()
#' #' bebie_stats <- calc_bebie(testdat1, field_var)
#' #' @return data frame
#' #' @export
#' field_variation <- function() {
#'   # testtype_unq <- unique(testdata$testtype)
#'   # if (length(testtype_unq) > 1) {
#'   #   stop("Too many tests. Create a list of unique tests.", call. = FALSE)
#'   # }
#'   data_split <- data_model %>%
#'     dplyr::filter(testtype != 'cr_diff') %>%
#'     mutate(value = MeanSens) %>%
#'     drop_na(value) %>%
#'     split(., interaction(.$testtype,.$patID))
#'
#'   interpolate_field <- function(x) {
#'     inter_test <- interpolate_norm(pred_data = x, grid_density = 0.25) %>%
#'       mutate_at(.vars = vars(x, y), .funs = round, digits = 2)
#'     inter_test
#'   }
#'   res_fields <- lapply(data_split, interpolate_field)
#'
#'   res_fields_bind <- bind_rows(res_fields, .id = "normID") %>%
#'     mutate(fit = round(fit, 2))
#'   res_fields_bind
#'   class(res_fields_bind) <- c("field_var","tbl_df", "tbl", "data.frame")
#'   res_fields_bind
#' }
#'
#' #' compare
#' #' @name compare
#' #' @description Interpolates normal values and compares with test values.
#' #' This is basically a wrapper around [lm_loc], [predict_norm],
#' #'   [coord_cart] and [interpolate_norm]
#' #' @param testdata **required**. Data frame with maia test results. See details
#' #' @details **testdata**: This should be ideally output from [read_maia].
#' #' The data frame must contain following columns:
#' #' **patID, eye, testID, testtype, eccent, angle, value, stimID**
#' #'
#' #' @import dplyr
#' #' @author tjebo
#' #' @return data frames with original and predicted normal values.
#' #' @export
#' compare <- function(testdata) {
#'
#'   if(inherits(testdata, 'list')){
#'
#'     vec_l <- lengths(lapply(testdata, function(x) unique(paste(x$testID, x$testtype))))
#'     if(!all(vec_l == 1)){
#'       stop('Too many tests. Create a list of unique tests.', call. = FALSE)
#'     }
#'     test_list <- testdata
#'   } else {
#'     testdata$test_unq <- paste(testdata$testID, testdata$testtype, sep = '_')
#'     test_list <- split(testdata, testdata$test_unq)
#'   }
#'   run_wrapper <- function(test_list_elem){
#'     list_lm <- lm_loc(test_list_elem)
#'     preddat <- predict_norm(test_list_elem, list_model = list_lm)
#'     testdat_coord <- coord_cart(test_list_elem)
#'     interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#'   }
#'
#'   interpol_list <- lapply(test_list, run_wrapper)
#'   class(interpol_list) <- c("compare","list")
#'   interpol_list
#' }
#'
#' #' Calculates microperimetry statistics.
#' #' @name mpstats
#' #' @description Calculates microperimetry statistics from a test or list of tests.
#' #'   This is basically a wrapper around [predict_norm], [coord_cart],
#' #'   [interpolate_norm] and [mpstats_single]
#' #'   Includes mean sensitivity (mean_sens), mean deviation (mean_dev),
#' #'   pattern standard deviation (psd).
#' #'   You can also pass a compare object directly (from compare function),
#' #'   thus saving time for duplicate kringing.
#' #' @details **mean_dev** and **psd** are estimated.
#' #'   This is, because there is no 'real mean' and, more importantly,
#' #'   no "real variance" of normal values for each given location.
#' #'   Mean and variance are estimated based on kringing (spatial interpolation)
#' #'   of *predicted (!)* values. The predicted values are given by simple linear
#' #'   regression for each location, based on the [norm_data].
#' #'   It includes age, sex and lens status as covariates.
#' #'   The variance used for estimation is equal to the *prediction intervals*
#' #'   from those models, assuming homoscedasticity in a non-weighted model
#' #' @seealso [stats::predict.lm], [predict_norm], [coord_cart],
#' #'   [interpolate_norm]
#' #' @param testdata microperimetry test data
#' #' @param digits Digits to be rounded to
#' #' @family stat functions
#' #' @return Matrix
#' #' @examples
#' #' test_compare <- compare(testdata)
#' #' mpstats(test_compare)
#' #' # or (same result)
#' #' mpstats(testdata)
#' #' @author tjebo
#' #' @export
#'
#' mpstats <- function(testdata, digits = 2) {
#'
#'   if(inherits(testdata, 'list')){
#'
#'     vec_l <- lengths(lapply(testdata, function(x) unique(paste(x$testID, x$testtype))))
#'     if(!all(vec_l == 1)){
#'       stop('Too many tests. Create a list of unique tests.', call. = FALSE)
#'     }
#'     test_list <- testdata
#'   } else {
#'     testdata$test_unq <- paste(testdata$testID, testdata$testtype, sep = '_')
#'     test_list <- split(testdata, testdata$test_unq)
#'   }
#'   #return(test_list)
#'   if(!inherits(testdata, "compare")){
#'     run_wrapper_full <- function(test_list_elem){
#'       preddat <- predict_norm(test_list_elem)
#'       testdat_coord <- coord_cart(test_list_elem)
#'       interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#'       mpstats <- mpstats_single(interpol_dat)
#'     }
#'     mpstats_list <- lapply(test_list, run_wrapper_full)
#'   } else {
#'     mpstats_list <- lapply(test_list, mpstats_single)
#'   }
#'   mpstats <- do.call(rbind, mpstats_list)
#'   rownames(mpstats) <- names(mpstats_list)
#'   mpstats
#' }
#'
#'
#' #' Microperimetry statistics.
#' #' @name mpstats_single
#' #' @description Calculates microperimetry statistics from a single test.
#' #'   Includes mean sensitivity (mean_sens), mean deviation (mean_dev),
#' #'   pattern standard deviation (psd). See also details.
#' #'   Works in conjunction with prediction functions. (see example)
#' #' @details **mean_dev** and **psd** are estimated.
#' #'   This is, because there is no 'real mean' and, more importantly,
#' #'   no "real variance" of normal values for each given location.
#' #'   Mean and variance are estimated based on kringing (spatial interpolation)
#' #'   of *predicted (!)* values. The predicted values are given by simple linear
#' #'   regression for each location, based on the [norm_data].
#' #'   It includes age, sex and lens status as covariates.
#' #'   The variance used for estimation is equal to the *prediction intervals*
#' #'   from those models, assuming homoscedasticity in a non-weighted model
#' #' @seealso [stats::predict.lm]
#' #' @param interpol_dat interpolated results from test data.
#' #' @param digits Digits to be rounded to
#' #' @family stat functions
#' #' @return vector
#' #' @examples
#' #' preddat <- predict_norm(testdat1)
#' #' testdat_coord <- coord_cart(testdat1)
#' #' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' #' mpstats_single(interpol_dat)
#' #' @author tjebo
#' #' @export
#'
#' mpstats_single <- function(interpol_dat, digits = 2) {
#'   # remove blind spot. Make sure this remains stimID for blind spot in maia import!!!
#'   data <- interpol_dat[interpol_dat$stimID != 0,]
#'   if (is.null(interpol_dat$fit) | is.null(interpol_dat$lwr) | is.null(interpol_dat$upr)) {
#'     stop('Data should contain columns \"fit\", \"lwr\" and \"upr\"
#'        Ideally, pass the result of interpolate_norm.')
#'   }
#'   interpol_dat$pred_int <- interpol_dat$upr - interpol_dat$lwr
#'
#'   testval <- interpol_dat$value
#'   normval <- interpol_dat$fit
#'   normvar <- interpol_dat$pred_int
#'
#'   mean_sens <- mean(testval, na.rm = TRUE)
#'   sd_sens <- stats::sd(testval, na.rm = TRUE)
#'   mean_dev <- mean((testval - normval) / normvar, na.rm = TRUE) /
#'     mean(1 / normvar, na.rm = TRUE)
#'
#'   psd <- sqrt(mean(normvar, na.rm = TRUE) *
#'                 (sum((testval - normval - mean_dev)^2 / normvar, na.rm = TRUE) /
#'                    (length(normvar) - 1)))
#'
#'   mpstats <-
#'     round(
#'       cbind( mean_sens = mean_sens,
#'              sd_sens = sd_sens,
#'              mean_dev = mean_dev,
#'              psd = psd),
#'       digits = digits
#'     )
#'   mpstats
#' }
#'
#' #' "Manual" calculation of microperimetry statistics
#' #' @name mpstats_manual
#' #' @author tjebo
#' #' @description
#' #' Calculates mean deviation (MD) and pattern standard deviation (psd)
#' #'   manually from data or vectors.
#' #' @param test_val observed sensitivities. Can be column name (quoted or unquoted) or numeric vector
#' #' @param norm_val mean of normal sensitivities of same length as test_val. Can be column name (quoted or unquoted) or numeric vector
#' #' @param var_norm variance of normal values of same length as test_val. Can be column name (quoted or unquoted) or numeric vector
#' #' @param data **optional** Dataframe with vectors for analysis
#' #' @family stat functions
#' #' @return named vector with mean deviation (mean_dev) and pattern standard deviation (psd)
#' #' @export
#' mpstats_manual <- function(test_val, norm_val, var_norm, data = NULL) {
#'   test_val_sub <- deparse(substitute(test_val))
#'   norm_val_sub <- deparse(substitute(norm_val))
#'   var_norm_sub <- deparse(substitute(var_norm))
#'
#'   if (test_val_sub %in% names(data)) {
#'     testval <- data[[test_val_sub]]
#'   } else if (is.atomic(test_val)) {
#'     testval <- test_val
#'   }
#'   if (norm_val_sub %in% names(data)) {
#'     normval <- data[[norm_val_sub]]
#'   } else if (is.atomic(norm_val)) {
#'     normval <- norm_val
#'   }
#'
#'   if (var_norm_sub %in% names(data)) {
#'     normvar <- data[[var_norm_sub]]
#'   } else if (is.atomic(var_norm)) {
#'     normvar <- var_norm
#'   }
#'   if (length(normval) != length(testval)) {
#'     stop("norm_val needs to be of same length as test_val")
#'   }
#'   if (length(normvar) != length(testval)) {
#'     stop("norm_var needs to be of same length as test_val")
#'   }
#'   mean_dev <- mean((testval - normval) / normvar, na.rm = TRUE) /
#'     mean(1 / normvar, na.rm = TRUE)
#'
#'   psd <- sqrt(mean(normvar, na.rm = TRUE) *
#'                 (sum((testval - normval - mean_dev)^2 / normvar, na.rm = TRUE) /
#'                    (length(normvar) - 1)))
#' }
#'
#' #' bootstrapping maia norm data
#' #' @name bootstrap_maia
#' #' @author tjebo
#' #' @description creates n bootstrap samples of the original norm_data
#' #' @param n number of bootstrap samples
#' #' @param remove_diff logical. TRUE will remove cr_diff data (default)
#' #' @import dplyr
#' #' @import tidyr
#' #' @importFrom rlang .data
#' #' @family stat functions
#' #' @examples
#' #' boots <- bootstrap_maia()
#' #' make linear model for each bootstrap sample
#' #' lm_boots <- lapply(boots, function(x) lm_loc(normdata = x))
#' #' # predict the location for each linear model.
#' #' # Using data of one test. But works also without testdata.
#' #' pred_boots <- lapply(
#' #'   lm_boots,
#' #'   function(x) predict_norm(testdata = testdat1, list_model = x)
#' #' )
#' #' # Creating the coordinates in testdat1 for the use in
#' #' # interpolate_norm
#' #' testdat_coord <- coord_cart(testdat1)
#' #'
#' #' # now create the interpolation maps for each bootstrap sample
#' #' interpol_boots <- lapply(
#' #'   pred_boots, function(x) {
#' #'     interpolate_norm(pred_data = x, newgrid = testdat_coord)
#' #'     }
#' #' )
#' #' @return List
#' #' @details Returns list of linear models
#' #' @export
#' #'
#' bootstrap_maia <- function(n = 50, remove_diff = TRUE) {
#'   if(remove_diff){
#'     data_boots <- data_model[data_model$testtype != "cr_diff", ]
#'   }
#'   length_repeat <- length(unique(data_boots$stimID)) * length(unique(data_boots$testtype))
#'   list_norm <- split(data_boots, data_boots$patID, drop = TRUE)
#'
#'   set.seed(41)
#'   sample_id <- as.data.frame(
#'     replicate(sample(length(unique(data_boots$patID)), replace = TRUE), n = n)
#'   )
#'
#'   boots <- lapply(sample_id, function(x) {
#'     boot_sample <- bind_rows(list_norm[x]) %>%
#'       arrange(.data$patID)
#'     boot_sample$patID <- rep(1:nrow(sample_id), each = length_repeat)
#'     boot_sample
#'   })
#'   boots
#' }
#'
#' #' calculates bebie statistics
#' #' @name calc_bebie
#' #' @author tjebo
#' #' @description linear regression model for each test location
#' #' of the grid underlying [norm_data], with age as predictor
#' #'  The output of this function is used for the prediction in
#' #' [predict_norm]
#' #' @param testdata test for which field variation will be calculated
#' #' @import dplyr
#' #' @import tidyr
#' #' @importFrom rlang .data
#' #' @family prediction functions
#' #' @examples
#' #' field_var <- field_variation()
#' #' bebie_stats <- calc_bebie(testdat1, field_var)
#' #' @return List
#' #' @details field variations should be created with [field_variations]
#' #' @export
#' calc_bebie <- function(testdata){
#'   testdat_coord <- coord_cart(testdata) %>%
#'     filter(stimID != 0) %>%
#'     mutate_at(.vars = vars(x, y), .funs=plyr::round_any, accuracy = 0.25) %>%
#'     mutate_at(.vars = vars(x, y), .funs= round, digits = 1)
#'
#'   testtest <- testdat_coord %>% left_join(field_var, by = c('testtype','x', 'y'))
#'
#'   cumdev_frame <-
#'     testtest %>%
#'     group_by(eccent, angle) %>%
#'     mutate(meansens = mean(fit)) %>%
#'     ungroup()
#'
#'   norm_cumdev <- cumdev_frame %>%
#'     group_by(normID) %>%
#'     mutate(locdev = fit - meansens,
#'            rank = rank(desc(locdev), ties.method = 'last')
#'     ) %>%
#'     group_by(rank) %>%
#'     summarise(mean = mean(locdev, na.rm = TRUE),
#'               upr = mean + 1.96 * sd(locdev, na.rm = TRUE),
#'               lwr = mean - 1.96 * sd(locdev, na.rm = TRUE)) %>%
#'     ungroup()
#'
#'   test_cumdev <- cumdev_frame %>%
#'     distinct(patID, testID, testtype, stimID, value, meansens) %>%
#'     mutate(locdev = value - meansens,
#'            rank = rank(desc(locdev), ties.method = 'last')
#'     )
#'   bebiestats <- left_join(test_cumdev, norm_cumdev, by = 'rank')
#'   class(bebiestats) <- c("bebie","tbl_df", "tbl", "data.frame")
#'   bebiestats
#' }
#'
#' #' bootstrapping maia norm data
#' #' @name bootstrap_maia
#' #' @author tjebo
#' #' @description creates n bootstrap samples of the original norm_data
#' #' @param n number of bootstrap samples
#' #' @param remove_diff logical. TRUE will remove cr_diff data (default)
#' #' @import dplyr
#' #' @import tidyr
#' #' @importFrom rlang .data
#' #' @family stat functions
#' #' @examples
#' #' boots <- bootstrap_maia()
#' #' make linear model for each bootstrap sample
#' #' lm_boots <- lapply(boots, function(x) lm_loc(normdata = x))
#' #' # predict the location for each linear model.
#' #' # Using data of one test. But works also without testdata.
#' #' pred_boots <- lapply(
#' #'   lm_boots,
#' #'   function(x) predict_norm(testdata = testdat1, list_model = x)
#' #' )
#' #' # Creating the coordinates in testdat1 for the use in
#' #' # interpolate_norm
#' #' testdat_coord <- coord_cart(testdat1)
#' #'
#' #' # now create the interpolation maps for each bootstrap sample
#' #' interpol_boots <- lapply(
#' #'   pred_boots, function(x) {
#' #'     interpolate_norm(pred_data = x, newgrid = testdat_coord)
#' #'     }
#' #' )
#' #' @return List
#' #' @details Returns list of linear models
#' #' @export
#' #'
#' bootstrap_maia <- function(n = 50, remove_diff = TRUE) {
#'   if(remove_diff){
#'     data_boots <- data_model[data_model$testtype != "cr_diff", ]
#'   }
#'   length_repeat <- length(unique(data_boots$stimID)) * length(unique(data_boots$testtype))
#'   list_norm <- split(data_boots, data_boots$patID, drop = TRUE)
#'
#'   set.seed(41)
#'   sample_id <- as.data.frame(
#'     replicate(sample(length(unique(data_boots$patID)), replace = TRUE), n = n)
#'   )
#'
#'   boots <- lapply(sample_id, function(x) {
#'     boot_sample <- bind_rows(list_norm[x]) %>%
#'       arrange(.data$patID)
#'     boot_sample$patID <- rep(1:nrow(sample_id), each = length_repeat)
#'     boot_sample
#'   })
#'   boots
#' }
#'
#'
#'
