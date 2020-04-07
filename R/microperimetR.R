#' microperimetr
#' Microperimetry analysis for centervue's maia and compass data
#' @name microperimetr
#' @docType package
#' @description This package will help you work with microperimetry data.
#'It is built for centervue's maia and compass devices.
#'    Extracting data from MAIA raw (tgz) files becomes as easy as pie.
#'    Fully automated comparison of your visual field data with normdata from the literature.
#'    microperimetr has functions for both location-dependent and location-independent visual field analysis. Because the normdata is interpolated for location-dependent analysis, any custom grid can be used for both types of analysis.
#'    microperimetr also includes functions for some more or less common visualistaion of visual field data
#' @author Tjebo Heeren and Maximilian Pfau and ... TBC!
#' @section plotting functions (TBC)
#' @section statistics and summary functions (TBC)
#'
#' @examples
#'
#' # Guide to plotting normal sensitivity data
#' # Includes linear regression lines
#' library(ggplot2)
#' ggplot(norm_data, aes(x = age, y = value)) +
#'   geom_point(aes(shape = lens), alpha = 0.05, show.legend = FALSE) +
#'   facet_wrap(~ testtype) +
#'   ylab("Sensitivity [dB]") +
#'   xlab("Age [years]") +
#'   geom_smooth(aes(color = testtype, linetype = lens), method = 'lm', formula = y ~ x) +
#'   scale_color_manual(name = 'Regression lines', values = maia_palette)
#'
#'# Calculate coefficient of repeatability of norm data.
#'cor_maia <- norm_data %>%
#'mutate(testnumber = paste0("E", .data$testnumber)) %>%
#'select(-'testID') %>%
#'pivot_wider(names_from = 'testnumber', values_from = 'value') %>%
#'mutate(diff_val = .data$E1-.data$E2, avg = mean(.data$E1+.data$E2)) %>%
#'group_by(.data$testtype) %>%
#'summarise(subj_sd = stats::sd(.data$diff_val, na.rm = TRUE), CoR = 1.96*sqrt(2)*.data$subj_sd)

NULL
