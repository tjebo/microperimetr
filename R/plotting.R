#' ## also for perimetry package
#'
#' #' plot_mpstats
#' #' @name plot_mpstats
#' #' @description plotting relative percentile of estimated Mean deviation for tested observers
#' #' @param testdata test data. Do not try to pass a list.
#' #'  Built to pass the output from [read_maia]
#' #' @param quantiles Quantiles for normal values to be displayed.
#' #' @return List of plots which are bound together with the patchwork package.
#' #' @author tjebo
#' #' @import ggplot2
#' #' @import tibble
#' #' @import dplyr
#' #' @import tidyr
#' #' @family microperimetry plotting functions
#' #' @examples
#' #' # testdata <- read_maia(folder = file.path(getwd(), "data-raw"))
#' #' # plot_mpstats(testdata)
#' #' @export
#' plot_mpstats <- function(testdata, quantiles = 0.1) {
#'
#'   norm_stats <- norm_mpstats %>%
#'     select(-"sd_sens") %>%
#'     filter(.data$testtype != 'crdiff', ) %>%
#'     pivot_longer(names_to = 'key', values_to = 'value', cols = .data$mean_sens:.data$psd) %>%
#'     group_by(.data$patID, .data$testtype, .data$key) %>%
#'     summarise(avg = mean(.data$value)) %>%
#'     mutate(key = gsub("_", "", .data$key),
#'            type_key = paste(.data$testtype, .data$key, sep = "_"),
#'            type = factor(.data$testtype,
#'                          levels = c("mesopic", "red", "cyan")),
#'            key = factor(.data$key, levels = c('meansens','meandev','psd'))) %>%
#'     ungroup
#'  norm_list <- split(norm_stats, norm_stats$type_key)
#'
#'   quantiles <- quantiles
#'   quantframe <-
#'     lapply(norm_list, function(x) {
#'       tibble::enframe(stats::quantile(x$avg, probs = c(seq(0, 1, by = quantiles))))
#'       }
#'       ) %>%
#'     bind_rows(.id = "type_key") %>%
#'     separate("type_key", c("type", "key")) %>%
#'     mutate(type = factor(.data$type, levels = c("mesopic", "red", "cyan", "crdiff")),
#'            key = factor(.data$key, levels = c('meansens','meandev','psd'))) %>%
#'     mutate(type_key = paste(.data$type, .data$key, sep = "_")) %>%
#'     group_by(.data$type_key) %>%
#'     mutate(x = min(.data$value), xend = max(.data$value)) %>%
#'     ungroup() %>%
#'     distinct(.data$type, .data$key, .data$name,.keep_all = TRUE)
#'
#'   segment_frame <-
#'     quantframe %>%
#'     distinct(.data$type, .data$key,.keep_all = TRUE)
#'
#'    mpstat_test<- mpstats(testdata) %>%
#'     as.data.frame() %>%
#'     tibble::rownames_to_column("testID_testtype") %>%
#'      select(-"sd_sens") %>%
#'     pivot_longer(names_to = 'key', values_to = 'value', cols = .data$mean_sens:.data$psd) %>%
#'     mutate(key = gsub("_", "", .data$key)) %>%
#'     separate(.data$testID_testtype, c("testID","type")) %>%
#'     mutate(type = factor(.data$type, levels = c("mesopic", "red", "cyan", "crdiff")),
#'            key = factor(.data$key, levels = c('meansens','meandev','psd'))) %>%
#'     mutate(type_key = paste(.data$type, .data$key, sep = "_"))
#'
#'   p <-
#'     ggplot(norm_stats, aes(.data$avg)) +
#'   geom_histogram(binwidth = 0.3, fill = NA, color = "grey") +
#'   geom_segment(
#'     data = mpstat_test,
#'     aes(x = .data$value, xend = .data$value,
#'         y = Inf, yend = -Inf, color = .data$type),
#'     size = 1.5, alpha = 0.5
#'   ) +
#'   geom_text(
#'     data = mpstat_test,
#'     aes(x = .data$value, y = 9,
#'         label = .data$testID, color = .data$type),
#'     size = 8 * 5 / 14,
#'     alpha = 0.5, hjust = 1, angle = 90, vjust = 0
#'   ) +
#'   geom_segment(data = segment_frame,
#'                aes(x = .data$x, xend = .data$xend,
#'                    y = Inf, yend = Inf)) +
#'   geom_text(
#'     data = quantframe,
#'     aes(.data$value, y = Inf, label = .data$name),
#'     angle = 90, size = 8 * 5 / 14,
#'     hjust = c(rep(c(0, 1), length(quantframe$value) / 2), 0)
#'   ) +
#'   scale_color_manual(values = maia_palette, guide = FALSE) +
#'   labs(y = "Count of normals", x = "Values") +
#'   coord_cartesian(clip = "off") +
#'   facet_grid(type ~ key, scales = "free") +
#'   theme_min(base_size = 8) +
#'   theme(
#'     strip.text = element_text(
#'       margin = margin(b = 20, unit = "pt"),
#'       hjust = 0, size = 8),
#'     panel.spacing.y = unit(20, 'pt')
#'   )
#'   p
#' }
#' #
#' # interpolation maps
#' # if (graph) {
#' #   plot_list <- list()
#' #   for (i in 1:length(norm_interpolated)) {
#' #     p <- ggplot(norm_interpolated[[i]], aes(x, y)) +
#' #       geom_raster(aes(fill = val_pred)) +
#' #       stat_contour(aes(z = val_pred), binwidth = 1, size = 0.1, color = "black") +
#' #       scale_fill_gradient(limits = c(-30, 30), low = "black", high = "white") +
#' #       labs(title = names(norm_interpolated)[i], x = "Temporal - nasal [1]", y = "Inferior - superior [1]", fill = "Sensitivity [dB]") +
#' #       coord_equal() +
#' #       theme_min()
#' #     plot_list[[i]] <- p
#' #   }
#' #
#' #   p_wrap <- patchwork::wrap_plots(plot_list, guides = "collect")
#' #   print(p_wrap)
#'
#' #' Plotting bland altman
#' #' @name plot_bland
#' #' @description Plotting bland altman plot of test retest of sMAIA device
#' #' Data from norm data
#' #' @param testdata needs to be data frame with two columns for each test.
#' #'   If not given, the data from sMAIA test retest will be shown
#' #' @param ... further arguments which will be passed to geom_point
#' #' @author tjebo
#' #' @import ggplot2
#' #' @import dplyr
#' #' @import tidyr
#' #' @importFrom rlang .data
#' #' @seealso [ggplot2::geom_point]
#' #' @family microperimetry plotting functions
#' #' @return ggplot object with side effect of printing to console
#' #' @export
#' #'
#' plot_bland <- function(testdata = NULL, ...) {
#'   if (is.null(testdata)) {
#'     data_wide <- microperimetr::norm_data %>%
#'       mutate(testnumber = paste0("E", .data$testnumber)) %>%
#'       select(-"testID") %>%
#'       pivot_wider(names_from = "testnumber", values_from = "value") %>%
#'       mutate(diff_val = .data$E1 - .data$E2, avg = (.data$E1 + .data$E2) / 2)
#'     sample_df <- data_wide %>% select("testtype", "E1", "E2", "diff_val", "avg")
#'     mean_diff <- mean(sample_df$diff_val, na.rm = TRUE)
#'     sd_diff <- stats::sd(sample_df$diff_val, na.rm = TRUE)
#'
#' p <- ggplot(
#'       sample_df,
#'       aes(
#'         x = .data$avg,
#'         y = .data$diff_val,
#'         group = .data$testtype
#'       )
#'     ) +
#'       geom_point(alpha = 0.1, position = position_jitter()) +
#'       facet_wrap(~ .data$testtype, scales = "free") +
#'       geom_hline(yintercept = mean_diff, size = 0.5) +
#'       geom_hline(
#'         yintercept = c(mean_diff - (1.96 * sd_diff), mean_diff + (1.96 * sd_diff)),
#'         linetype = 2, size = 0.5
#'       ) +
#'       labs(x = "Mean [dB]", y = "Difference [dB]") +
#'       theme_min()
#'   } else {
#'     testdata$diff_val <- testdata[[1]] - testdata[[2]]
#'     testdata$avg <- (testdata[[1]] + testdata[[2]]) / 2
#'     mean_diff <- mean(testdata$diff_val, na.rm = TRUE)
#'     sd_diff <- stats::sd(testdata$diff_val, na.rm = TRUE)
#'
#'     p <- ggplot(testdata, aes(x = .data$avg, y = .data$diff_val)) +
#'       geom_point(position = position_jitter(), ...) +
#'       geom_hline(yintercept = mean_diff, size = 0.5) +
#'       geom_hline(
#'         yintercept = c(mean_diff - (1.96 * sd_diff), mean_diff + (1.96 * sd_diff)),
#'         linetype = 2, size = 0.5
#'       ) +
#'       labs(x = "Mean [dB]", y = "Difference [dB]") +
#'       theme_min()
#'   }
#'   p
#' }
#'
#' #' Plotting bebie curves
#' #' @name plot_bebie
#' #' @description Plotting bebie curve. For details of calculation of bebie stats,
#' #'   see [field_variation] and [calc_bebie]
#' #' @param data Data for which bebie curve will be calculated. Can be either
#' #'   the original test data or the output from [calc_bebie]
#' #' @param ... further arguments which will be passed to geom_line for the
#' #'   test data
#' #' @author tjebo
#' #' @import ggplot2
#' #' @importFrom rlang .data
#' #' @seealso [ggplot2::geom_line]
#' #' @examples
#' #' field_var <- field_variation()
#' #' bebie_stats <- calc_bebie(testdat1)
#' #' plot_bebie(bebie_stats)
#' #' @family microperimetry plotting functions
#' #' @return ggplot object with side effect of printing to console
#' #' @export
#' #'
#' #'
#' plot_bebie <- function(data, ...) {
#'
#'   if(!inherits(data, 'bebie')){
#'   field_var <- field_variation()
#'   bebie_stats <- calc_bebie(data)
#'   } else {
#'     bebie_stats <- data
#'   }
#'   p <- ggplot(bebie_stats, aes(x = rank)) +
#'     geom_ribbon(aes(ymin = lwr, ymax = upr, color = testtype, fill = testtype),
#'                 alpha = 0.3, size = 0.2,
#'                 ) +
#'     geom_line(aes(y = mean), linetype = "dashed") +
#'     geom_line(aes(y = locdev, color = testtype), ...) +
#'     scale_color_manual(values = maia_palette) +
#'     scale_fill_manual(values = maia_palette, guide = FALSE) +
#'     labs(color = "Test type") +
#'     theme_min()
#'
#'
#'   p
#' }
#'
