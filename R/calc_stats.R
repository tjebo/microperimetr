#' pred_location
#'
#' @author tjebo
#'
#' @description location based prediction of retinal sensitivity using a linear model for each location with age, sex and lens status as covariates
#' Note this function is used under the hood for the prediction of interpolated values.
#' You can use it for prediction of retinal sensitivity at one of the test locations which underly the normal data, but it would probably make more sense to create a mixed effect model, taking into account the within-subject-variance
#' E.g. lme4::lmer(value ~ age + sex + lens + (1|patID))
#' @param age required age of individual to be predicted
#' @param sex required sex of individual to be predicted
#' @param lens lens status. must be either 'natural' (default) or 'pseudo'
#' @param interval used in [stats::predict.lm()]
#'
#' @return Data frame with predicted sensitivity values for all test types at each location
#' @export


pred_location <- function(age, sex, lens = 'natural', interval = 'confidence'){
  maiaR:::using('tidyverse')
  if(!lens%in% c('natural','pseudo')) stop('lens needs to be either \'natural\' or \'pseudo\')')
  if(!sex%in% c('m','f')) stop('sex needs to be either \'m\' or \'f\')')

  newdata <- data.frame(age = age, sex = sex, lens = lens)
  split_vec <-interaction(maiaR:::data_model$testtype, maiaR:::data_model$position)
  #  putting test and retest in two columns for
  # linear modelling, icc and kendall calculation
  data_list <- maiaR:::data_model %>% split(f = split_vec)
  #order list so that it has same order as unique splits
  data_list <- data_list[unique(split_vec)]
  list_lm <- lapply(data_list, function(x) lm(MeanSens ~ age + lens + sex, data = x))
  res_pred <- do.call(rbind, lapply(list_lm, function(x) predict(x, newdata = newdata, interval = interval)))
  #give the names back
  rownames(res_pred) <- names(list_lm)

  results <- res_pred %>% as.data.frame() %>% rownames_to_column('test.position') %>%
    separate(test.position, c('testtype', 'position'), sep = '\\.') %>%
    separate(position, c("eccent", "angle"))

  return(results)
}

#' Coefficient of repeatability
#' @description Coefficient of repeatability of norm data
#' Calculated with formula CoR = 1.96*sqrt(2)* sqrt(within-subject-variance)
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
  maiaR:::using('tidyverse')

  data_wide <- maiaR::norm_data %>%
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
    labs(x = 'Mean [dB]', y = "Difference [dB]")

  if(graph) print(p); warning('mesopic testing of one individual contains NA')

  return(CoR)
}

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
