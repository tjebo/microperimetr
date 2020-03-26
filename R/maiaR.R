#' maiaR
#'
#' For working with MAIA raw data
#' @name maiaR
#' @docType package
#' @author Tjebo Heeren and Maximilian Pfau
#'
#' @examples
#' # Guide to plotting normal sensitivity data
#' # Includes linear regression lines
#' ggplot(norm_data, aes(x = Age, y = value)) +
#'   geom_point(aes(x = Age, y = value, shape = lens), alpha = 0.05, show.legend = FALSE) +
#'   facet_wrap(~ testtype) +
#'   ylab("Sensitivity [dB]") +
#'   xlab("Age [years]") +
#'   geom_smooth(aes(color = testtype, linetype = lens), method = 'lm', formula = y ~ x) +
#'   scale_color_manual(name = 'Regression lines', values = maia_palette)
NULL
