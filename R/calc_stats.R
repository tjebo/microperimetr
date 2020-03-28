#' pred_location
#' @name pred_location
#' @author tjebo
#' @description location based prediction of retinal sensitivity using a linear model for each location with age, sex and lens status as covariates
#' Note this function is used under the hood for the prediction of interpolated values.
#' You can use it for prediction of retinal sensitivity at one of the test locations which underly the normal data, but it would probably make more sense to create a mixed effect model, taking into account the within-subject-variance
#' E.g. lme4::lmer(value ~ age + sex + lens + (1|patID))
#' @param age age of individual to be predicted. defaults to 50 if none given
#' @param sex sex of individual to be predicted. defaults to 'm' if none given
#' @param lens lens status. must be either 'natural' (default) or 'pseudo'. defaults to 'natural' if none given
#' @param interval used in \link[stats]{predict.lm}
#'
#' @return Data frame with predicted sensitivity values for all test types at each location
#' @export

pred_location <- function(age = NULL, sex = NULL, lens = NULL, interval = 'confidence'){
  microperimetR:::using('tidyverse')
  if(is.null(age)) {warning('age is not set - default age of 50 years will be used'); age <- 50}
  if(is.null(sex)) {warning('sex is not set - default sex \'m\' will be used'); sex <- 'm'}
  if(is.null(lens)) {warning('lens is not set - default lens \'natural\' be used'); lens <- 'natural'}

  if(!lens%in% c('natural','pseudo')) stop('lens needs to be either \'natural\' or \'pseudo\')')
  if(!sex%in% c('m','f')) stop('sex needs to be either \'m\' or \'f\')')

  newdata <- data.frame(age = age, sex = sex, lens = lens)
  split_vec <-interaction(microperimetR:::data_model$testtype, microperimetR:::data_model$position)
  #  putting test and retest in two columns for
  # linear modelling, icc and kendall calculation
  data_list <- microperimetR:::data_model %>% split(f = split_vec)
  #order list so that it has same order as unique splits
  data_list <- data_list[unique(split_vec)]
  list_lm <- lapply(data_list, function(x) lm(MeanSens ~ age + lens + sex, data = x))
  res_pred <- do.call(rbind, lapply(list_lm, function(x) predict(x, newdata = newdata, interval = interval)))
  #give the names back
  rownames(res_pred) <- names(list_lm)

  results <- res_pred %>% as.data.frame() %>% rownames_to_column('test.position') %>%
    separate(test.position, c('testtype', 'position'), sep = '\\.') %>%
    separate(position, c("eccent", "angle")) %>%
    mutate_at(.vars = vars(eccent, angle), .funs = 'as.numeric')

  return(results)
}

#' interpolate_norm
#' @name interpolate_norm
#' @description Interpolates grid based on local predictions of normal data.
#'
#' @author tjebo and max
#' @param age age of individual to be predicted. passed to \link{pred_location}
#' @param sex sex of individual to be predicted. passed to \link{pred_location}
#' @param lens lens status. must be either 'natural' (default) or 'pseudo'. passed to \link{pred_location}
#' @param interval used in \link[stats]{predict.lm}
#' @param as_df FALSE (default): list of data frames will be returnedif TRUE, value of data frame will be returned.
#' @param grid_density resolution of interpolated grid in degress.
#' @param graph if TRUE, the interpolation will be plotted for all test types
#'
#' @return List of data frames with predicted results for grid for all test types
#' @examples
#' and plot if graph = TRUE
#' # Tests were performed
#'  plot(gstat::variogramLine(fit_fit, 10), type='l')
#'  points(vario[,2:3], pch=20, col='red')
#' LOOCV of data points from interpol_dat data
#' x <- gstat::krige.cv(fit ~ 1, interpol_dat, interpol_dat, model = fit_fit, nfold = nrow(interpol_dat))
#' RMSE(x$var1.pred,x$observed)
#' vgm.fit
#' plot(vgm, vgm.fit)
#' sp::spplot(mes_ok_mean ["var1.pred"])
#' @export
#'
interpolate_norm <- function(age = NULL, sex = NULL, lens = NULL, interval = "confidence", as_df = FALSE, grid = NULL, grid_density = 0.2, graph = FALSE) {
  microperimetR:::using("tidyverse", "gstat", "sp")
  # x - y coordinates
  results <- microperimetR::pred_location(age = age, sex = sex, lens = lens, interval = interval) %>%
    mutate(
      angle = as.integer(angle),
      x = cos(angle * pi / 180) * as.numeric(eccent),
      y = sin(angle * pi / 180) * as.numeric(eccent)
    )

  # create grid
  if (is.null(grid)) {
    dense_grid <- expand.grid(x = seq(-10, 10, by = grid_density), y = seq(-10, 10, by = grid_density)) %>%
      mutate(eccent = sqrt(x^2 + y^2))
    sp_grid <- dense_grid
    sp::coordinates(sp_grid) <- ~ x + y
    sp::gridded(sp_grid) <- TRUE
  } else {
    dense_grid <- grid %>%
      mutate(
        angle = as.integer(angle),
        x = cos(angle * pi / 180) * as.numeric(eccent),
        y = sin(angle * pi / 180) * as.numeric(eccent)
      )
    sp_grid <- dense_grid
    sp::coordinates(sp_grid) <- ~ x + y
  }

  list_res <- results %>% split(results$testtype)
  interpolate_predictions <- function(x) {
    interpol_dat <- x %>% select(x, y, "fit", "lwr", "upr")
    sp::coordinates(interpol_dat) <- ~ x + y
    # fit with inverted distance weighting
    gstat_obj <- gstat::gstat(formula = fit ~ 1, data = interpol_dat)
    vario <- gstat::variogram(gstat_obj)
    # vgm fitting with set of models, returning the better fitting model
    fit_fit <- gstat::fit.variogram(vario, gstat::vgm(c("Exp", "Sph", "Mat")))
    mes_fit <- gstat::krige(fit ~ 1, interpol_dat, sp_grid, fit_fit)
    # lwr
    gstat_obj <- gstat::gstat(formula = lwr ~ 1, data = interpol_dat)
    vario <- gstat::variogram(gstat_obj)
    fit_lwr <- gstat::fit.variogram(vario, gstat::vgm(c("Exp", "Sph", "Mat")))
    mes_lwr <- gstat::krige(fit ~ 1, interpol_dat, sp_grid, fit_lwr)
    # upr
    gstat_obj <- gstat::gstat(formula = upr ~ 1, data = interpol_dat)
    vario <- gstat::variogram(gstat_obj)
    fit_upr <- gstat::fit.variogram(vario, gstat::vgm(c("Exp", "Sph", "Mat")))
    mes_upr <- gstat::krige(fit ~ 1, interpol_dat, sp_grid, fit_upr)

    output <- cbind(select(dense_grid,-testtype),
      val_pred = mes_fit$var1.pred, val_var = mes_fit$var1.var,
      lwr_pred = mes_lwr$var1.pred, lwr_var = mes_lwr$var1.var,
      upr_pred = mes_upr$var1.pred, upr_var = mes_upr$var1.var
    )
    output
  }

  norm_interpolated <- lapply(list_res, interpolate_predictions)[c("mesopic", "cyan", "red", "cr_diff")]
  if (as_df) norm_interpolated <- norm_interpolated %>% bind_rows(.id = 'testtype')

  if (graph) {
    plot_list <- list()
    for (i in 1:length(res_interpol)) {
      p <- ggplot(res_interpol[[i]], aes(x, y)) +
        geom_raster(aes(fill = val_pred)) +
        stat_contour(aes(z = val_pred), binwidth = 1, size = 0.1, color = "black") +
        scale_fill_gradient(limits = c(-30, 30), low = "black", high = "white") +
        labs(title = names(res_interpol)[i], x = "Temporal - nasal [1]", y = "Inferior - superior [1]", fill = "Sensitivity [dB]") +
        coord_equal() +
        theme_min()
      plot_list[[i]] <- p
    }

    p_wrap <- patchwork::wrap_plots(plot_list, guides = "collect")
    print(p_wrap)
  }

  return(norm_interpolated)
}

#' compare_norm
#' @name compare_norm
#' @description Interpolates grid based on local predictions of normal data.
#' For test points that correspond with basic normal grid, the values from \link{pred_location} will be used.
#' For test points outside the normal grid location, values from \link{interpolate_norm} will be used
#' @param test_data **required**. data frame with eccentricity and degree and corresponding test value
#' @param age age of individual to be predicted. passed to \link{pred_location}
#' @param sex sex of individual to be predicted. passed to \link{pred_location}
#' @param lens lens status. must be either 'natural' (default) or 'pseudo'. passed to \link{pred_location}
#' @param interval used in [stats::predict.lm]
#' @author tjebo
#' @return Data frame with original and predicted normal values.
#' @md
#' @export
compare_norm <- function(test_data, age = NULL, sex = NULL, lens = NULL, interval = 'confidence'){
  maiaR:::using('tidyverse')

  test_locations <- unique(microperimetR:::data_model$position)
  ident_loc <- test_data[paste(test_data$eccent, test_data$angle, sep = '_') %in% test_locations,]
  non_ident_loc <- test_data[!paste(test_data$eccent, test_data$angle, sep = '_') %in% test_locations,]

  age <- as.numeric(unique(test_data$age))
  sex <- unique(test_data$sex)

  results <- pred_location(age = age, sex = sex, lens = lens, interval = interval) %>%
    mutate(
      angle = as.integer(angle),
      x = cos(angle * pi / 180) * as.numeric(eccent),
      y = sin(angle * pi / 180) * as.numeric(eccent)
    )

  ident_norm_pred <- results %>% right_join(ident_loc, by = c('eccent','angle', 'testtype'))

  nonident_norm_pred <-
    interpolate_norm(age = age, lens = lens, sex = sex, interval = interval,
                     grid = non_ident_loc, as_df = TRUE)   %>%
    select(testtype, eccent, angle, x, y,
           fit = val_pred, lwr = lwr_pred, upr = upr_pred) %>%
    right_join(non_ident_loc, by = c('testtype', 'eccent','angle'))
  names_l <- list(names(ident_norm_pred), names(nonident_norm_pred))
  #return(names_l)
  new_frame <- bind_rows(ident_norm_pred, nonident_norm_pred) %>%
    select(patID, sex, age, testID, testtype, everything())
}

#' CoR_maia
#' @name CoR_maia
#' @description Coefficient of repeatability of norm data
#' Calculated with formula \eqn{CoR = 1.96* \sqrt2*\sqrt(within-subject-variance)}
#'
#' @author tjebo
#'
#' @param graph if TRUE, bland altman plot will be printed (with error lines at +/- 1.96*sd)
#'
#' @return Data frame with coefficients of repeatability for all test types
#' and plot if graph = TRUE
#' @export
#'
CoR_maia <- function(graph = TRUE){
  microperimetR:::using('tidyverse')

  data_wide <- microperimetR::norm_data %>%
    mutate(testnumber = paste0("E", testnumber)) %>%
    pivot_wider(names_from = 'testnumber', values_from = 'value') %>%
    mutate(diff_val = E1-E2, avg = (E1+E2)/2)

  CoR <- data_wide %>%
    group_by(testtype) %>%
    summarise(subj_sd = sd(diff_val, na.rm = TRUE), CoR = 1.96*sqrt(2)*subj_sd)

  sample_df <- data_wide %>% select(testtype, E1, E2, diff_val, avg)
  mean_diff <- mean(sample_df$diff_val, na.rm = TRUE)
  sd_diff <-  sd(sample_df$diff_val, na.rm = TRUE)

  p <- ggplot(sample_df, aes(x = avg, y = diff_val, group = testtype)) +
    geom_point(alpha = 0.1, position = position_jitter()) +
    facet_wrap(~ testtype, scales = 'free') +
    geom_hline(yintercept = mean_diff, size = 0.5) +
    geom_hline(yintercept = c(mean_diff - (1.96 * sd_diff), mean_diff + (1.96 * sd_diff)), linetype = 2, size = 0.5) +
    labs(x = 'Mean [dB]', y = "Difference [dB]") +
    theme_min()

  if(graph) print(p); warning('mesopic testing of one individual contains NA')

  return(CoR)
}

#' calc_globInd
#' @name calc_globInd
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
