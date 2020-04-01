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

pred_location <- function(age = NULL, sex = NULL, lens = NULL, interval = "confidence") {

  if (is.null(age)) {
    warning("age is not set - default age of 50 years will be used")
    age <- 50L
  }
  if (is.null(sex)) {
    warning("sex is not set - default sex 'm' will be used")
    sex <- "m"
  }
  if (is.null(lens)) {
    warning("lens is not set - default lens 'natural' be used")
    lens <- "natural"
  }

  if (!lens %in% c("natural", "pseudo")) stop("lens needs to be either 'natural' or 'pseudo')")
  if (!sex %in% c("m", "f")) stop("sex needs to be either 'm' or 'f')")

  split_vec <- interaction(data_model$testtype, data_model$position)

  #  putting test and retest in two columns for linear modelling, icc and kendall calculation
  data_list <- data_model %>% split(f = split_vec)

  # order list so that it has same order as unique splits
  data_list <- data_list[unique(split_vec)]
  list_lm <- lapply(data_list, function(x) stats::lm(MeanSens ~ age + lens + sex, data = x))

  newdata <- data.frame(age = age, sex = sex, lens = lens)

  res_pred <- lapply(list_lm, function(x) stats::predict(x, newdata = newdata, interval = interval))
  res_pred <- do.call(rbind, lapply(res_pred, unlist))
  rownames(res_pred) <- names(list_lm)

  results <- res_pred %>%
    as.data.frame() %>%
    rownames_to_column("test.position") %>%
    separate(test.position, c("testtype", "position"), sep = "\\.") %>%
    separate(position, c("eccent", "angle")) %>%
    mutate_at(.vars = vars(eccent, angle), .funs = "as.numeric")
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
#' @param newgrid custom grid to be interpolated on. If not specified, regular grid will be interpolated with resolution given in grid_density
#' @param grid_density resolution of interpolated grid in degress.
#' @param graph if TRUE, the interpolation will be plotted for all test types
#'
#' @return List of data frames with predicted results for grid for all test types and plot if graph = TRUE
#' @examples
#'
#' # Tests were performed
#' # plot(gstat::variogramLine(fit_fit, 10), type='l')
#' # points(vario[,2:3], pch=20, col='red')
#' #LOOCV of data points from interpol_dat data
#' #x <- gstat::krige.cv(fit ~ 1, interpol_dat, interpol_dat,
#' #       model = fit_fit, nfold = nrow(interpol_dat))
#' #RMSE(x$var1.pred,x$observed)
#' #vgm.fit
#' #plot(vgm, vgm.fit)
#' #sp::spplot(mes_ok_mean ["var1.pred"])
#' @export
#'
interpolate_norm <- function(age = NULL, sex = NULL, lens = NULL, interval = "confidence", as_df = FALSE, newgrid = NULL, grid_density = 0.2, graph = FALSE) {
  using("gstat", "sp")

  # x - y coordinates
  results <- microperimetR::pred_location(age = age, sex = sex, lens = lens, interval = interval) %>%
    mutate(
      angle = as.integer(angle),
      x = cos(angle * pi / 180) * as.numeric(eccent),
      y = sin(angle * pi / 180) * as.numeric(eccent)
    )
  # create grid
  if (!is.null(newgrid)) {
    dense_grid <- newgrid %>% select(eccent, angle) %>%
      mutate(x = cos(angle * pi / 180) * as.numeric(eccent),
             y = sin(angle * pi / 180) * as.numeric(eccent))
    sp_grid <- dense_grid
    sp::coordinates(sp_grid) <- ~ x + y} else if (is.null(newgrid)) {
    dense_grid <- expand.grid(x = seq(-10, 10, by = grid_density), y = seq(-10, 10, by = grid_density))
    sp_grid <- dense_grid
    sp::coordinates(sp_grid) <- ~ x + y
    sp::gridded(sp_grid) <- TRUE
  }

  # if no testdata given, will always produce prediction for all testtypes!!!
  # Don't get confused in the other fucntions which may get the entire set as an intermediate outpu
  if (is.null(newgrid)) {
  list_res <- results %>% split(results$testtype)
} else {
  list_res <- filter(results, testtype == unique(newgrid$testtype)) %>% split(.$testtype)
}

  interpolate_predictions <- function(dat) {
    interpol_dat <- dat %>% select(x, y, "fit", "lwr")
    sp::coordinates(interpol_dat) <- ~ x + y

    # fit with inverted distance weighting
    # vgm fitting with set of models, returning the better fitting model
    # fit (mean sensitivity)
    gstat_obj <- gstat::gstat(formula = fit ~ 1, data = interpol_dat)
    vario <- gstat::variogram(gstat_obj)
    fit_fit <- gstat::fit.variogram(vario, gstat::vgm(c("Exp", "Sph", "Mat")))
    mes_fit <- gstat::krige(fit ~ 1, interpol_dat, sp_grid, fit_fit)
    # lwr (lower value of error interval)
    gstat_obj <- gstat::gstat(formula = lwr ~ 1, data = interpol_dat)
    vario <- gstat::variogram(gstat_obj)
    fit_lwr <- gstat::fit.variogram(vario, gstat::vgm(c("Exp", "Sph", "Mat")))
    mes_lwr <- gstat::krige(lwr ~ 1, interpol_dat, sp_grid, fit_lwr)

    output <- cbind(dense_grid,
      val_pred = mes_fit$var1.pred, val_var = mes_fit$var1.var,
      lwr_pred = mes_lwr$var1.pred, lwr_var = mes_lwr$var1.var
    ) %>%
      mutate(upr_pred = val_pred + (val_pred - lwr_pred))
    output
  }

  norm_interpolated <- lapply(list_res, interpolate_predictions)

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
#' @param testresults **required**. data frame with sensitivity values. See details.
#' Data argument should be data frame only! List of data frames could result in unexpected output.
#' The data frame is ideally taken directly from [read_maia] output, ideally joined with information on lens status. (not contained in read_maia() output!)
#' It should at least contain following columns: **patID, eye, testID, testtype, eccent, angle, value, stimID**
#' It should ideally also contain **age, sex and lens** - If not, defaults of age, sex and lens of \link{pred_location} will be used
#' @param interval used in [stats::predict.lm]
#' @author tjebo
#' @return list of data frames (for each testID) with original and predicted normal values.
#' @export
compare_norm <- function(testresults, interval = "predict") {
  test_list <- testresults %>% split(., testresults$testID)

  test_locations <- as.character(unique(data_model$position))

  run_list <- function(test_data) {
    ident_loc <- test_data[paste(test_data$eccent, test_data$angle, sep = "_") %in% test_locations, ] %>% select(-testID)
    non_ident_loc <- test_data[!paste(test_data$eccent, test_data$angle, sep = "_") %in% test_locations, ] %>% select(-testID)

    if ("age" %in% names(test_data)) {
      age <- as.numeric(unique(test_data$age))
    } else {
      age <- NULL
    }

    if ("sex" %in% names(test_data)) {
      sex <- as.character(unique(test_data$sex))
    } else {
      sex <- NULL
    }
    if ("lens" %in% names(test_data)) {
      lens <- as.character(unique(test_data$lens))
    } else {
      lens <- NULL
    }

    results <- microperimetR::pred_location(age = age, sex = sex, lens = lens, interval = interval) %>%
      mutate(
        angle = as.integer(angle),
        x = cos(angle * pi / 180) * as.numeric(eccent),
        y = sin(angle * pi / 180) * as.numeric(eccent)
      )
    ident_norm_pred <- results %>% right_join(ident_loc, by = c("eccent", "angle", "testtype"))

    if (nrow(non_ident_loc) > 0) {
      nonident_norm_pred <- interpolate_norm(
        age = age, lens = lens, sex = sex, interval = interval,
        newgrid = non_ident_loc, as_df = TRUE
      ) %>%
        select(testtype, eccent, angle, fit = val_pred, lwr = lwr_pred, upr = upr_pred) %>%
        right_join(non_ident_loc, by = c("testtype", "eccent", "angle"))
      new_frame <- bind_rows(ident_norm_pred, nonident_norm_pred)
    } else {
      new_frame <- ident_norm_pred
    }

    new_frame <- new_frame %>% select(patID, sex, age, eye, testtype, eccent, angle, stimID, value, fit, lwr, upr)
  }
  list_res <- lapply(test_list, run_list) %>% bind_rows(.id = "testID")

  list_res
}

#' CoR_maia
#' @name CoR_maia
#' @description Coefficient of repeatability of norm data
#' Calculated with formula \eqn{CoR = 1.96* \sqrt2*\sqrt(within-subject-variance)}
#' @author tjebo
#'
#' @param graph if TRUE, bland altman plot will be printed (with error lines at +/- 1.96*sd)
#' @import dplyr
#' @import tidyr
#' @return Data frame with coefficients of repeatability for all test types
#' and plot if graph = TRUE
#' @export
#'
CoR_maia <- function(graph = TRUE){

  data_wide <- microperimetR::norm_data %>%
    mutate(testnumber = paste0("E", testnumber)) %>%
    select(-testID) %>%
    pivot_wider(names_from = 'testnumber', values_from = 'value') %>%
    mutate(diff_val = E1-E2, avg = (E1+E2)/2)

  CoR <- data_wide %>%
    group_by(testtype) %>%
    summarise(subj_sd = stats::sd(diff_val, na.rm = TRUE), CoR = 1.96*sqrt(2)*subj_sd)

  sample_df <- data_wide %>% select(testtype, E1, E2, diff_val, avg)
  mean_diff <- mean(sample_df$diff_val, na.rm = TRUE)
  sd_diff <-  stats::sd(sample_df$diff_val, na.rm = TRUE)

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

#' MD_PSD
#' @name MD_PSD
#' @description calculates estimated mean deviation (MD). This is estimated, because there is no 'real mean' of normal values for each location.
#' The mean and variance are taken from the local linear regression taken from the normal data, including age, sex and lens status as covariates.
#' Variance is estimated with the [stats::predict.lm] function, assuming homoscedasticity in a non-weighted linear regression with equal variance or residuals.
#' The estimated variance is (currently) equal to the prediction interval
#' @param data data of
#' @return named vector with mean deviation (MDev) and pattern standard deviation (PSD)
#' @author tjebo
#' @export
MD_PSD <- function(data) {
  data <- data %>% filter(stimID != 0) # remove blind spot. Make sure this remains stimID for blind spot in maia import!!!
if(all(c('fit','lwr','upr') %in% names(data))){
  if(sum(is.na(c(data$fit, data$lwr, data$upr))>0)) stop('there are missing values in your fitted values. Pass data without missing values')
  comparedat <- data
} else {
  comparedat <- compare_norm(testresults = data, interval = "predict")
}

  comparedat <- comparedat %>%
    mutate(pred_int = upr - lwr) %>%
    split(.$testID)
  extractMD <- function(comparedat) {
    testval <- comparedat$value
    normval <- comparedat$fit
    normvar <- comparedat$pred_int

    MDev <- mean((testval - normval) / normvar, na.rm = TRUE) /
      mean(1 / normvar, na.rm = TRUE)

    PSD <- sqrt(mean(normvar, na.rm = TRUE) *
      (sum((testval - normval - MDev)^2 / normvar, na.rm = TRUE) / (length(normvar) - 1)))

    MDresult <- data.frame(MeanDev = MDev, PSD = PSD, testtype = unique(comparedat$testtype), stringsAsFactors = FALSE)
    MDresult
  }
  res_MD <- lapply(comparedat, extractMD) %>%
    bind_rows(.id = 'testID')
  res_MD
}

#' MD_PSD_man
#' @name MD_PSD_man
#' @author tjebo
#' @description calculates global indices mean deviation (MD) and pattern standard deviation (PSD) manually from data or vectors
#' @param test_val observed sensitivities. Can be column name (quoted or unquoted) or numeric vector
#' @param norm_val mean of normal sensitivities of same length as test_val. Can be column name (quoted or unquoted) or numeric vector
#' @param var_norm variance of normal values of same length as test_val. Can be column name (quoted or unquoted) or numeric vector
#' @param data **optional** Dataframe with vectors for analysis
#'
#' @return named vector with mean deviation (MDev) and pattern standard deviation (PSD)
#' @export
MD_PSD_man <- function (test_val, norm_val, var_norm, data = NULL) {
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
  if(length(normval)!=length(testval)) stop('norm_val needs to be of same length as test_val')
  if(length(normvar)!=length(testval)) stop('norm_var needs to be of same length as test_val')

  MDev <- mean((testval - normval)/ normvar, na.rm = TRUE)/
    mean(1/normvar, na.rm = TRUE)

  PSD <- sqrt(mean(normvar, na.rm = TRUE) *
                (sum((testval-normval-MDev)^2/normvar, na.rm = TRUE)/(length(normvar)-1)))
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



