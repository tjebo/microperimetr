#' plot_MD
#' @name plot_MD
#' @description plotting relative percentile of estimated Mean deviation for tested observers
#' @param testdata test data. Do not try to pass a list. Built to pass the output from [read_maia] or, even better, from [compare_norm]
#' If data is organised in list of data frames, bind those first.
#' @param quantiles Quantiles for normal values to be displayed.
#' @return List of plots which are bound together with the patchwork package.
#' @author tjebo
#' @examples
#' testdata <- read_maia(folder = 'https://github.com/tjebo/microperimetR/data-raw')
#' comparedat <- compare_norm(testdata)
#' plot_MD(comparedat)
#' @export
plot_MD <- function(testdata, quantiles = 0.1) {

  MD_list <- MD_PSD(testdata) %>%
    split(.$testtype) %>%
    bind_rows(.id = "type") %>%
    mutate(type = factor(type, levels = c("mesopic", "red", "cyan", "cr_diff")))

  unique_type <- unique(testdata$testtype)
  names(unique_type) <- unique_type

  list_norm_MD <- lapply(unique_type, function(x) summary_MDPSD[[x]])
  norm_MD <- list_norm_MD %>%
    bind_rows(.id = "type") %>%
    mutate(type = factor(type, levels = c("mesopic", "red", "cyan", "cr_diff")))

  quantframe_list <- lapply(list_norm_MD, function(x) enframe(stats::quantile(x$MeanDev, probs = c(seq(0, 1, by = quantiles))))) %>%
    bind_rows(.id = "type") %>%
    mutate(type = factor(type, levels = c("mesopic", "red", "cyan", "cr_diff"))) %>%
    group_by(type) %>%
    mutate(x = min(value), xend = max(value)) %>%
    ungroup()

  p <-
    ggplot(norm_MD, aes(MeanDev)) +
    geom_histogram(binwidth = 0.3, fill = NA, color = "grey") +
    geom_segment(
      data = MD_list, aes(x = MeanDev, xend = MeanDev, y = Inf, yend = -Inf, color = type),
      size = 1.5, alpha = 0.5
    ) +
    geom_text(
      data = MD_list, aes(x = MeanDev, y = 9, label = testID, color = type),
      size = 8 * 5 / 14,
      alpha = 0.5, hjust = 1, angle = 90, vjust = 0
    ) +
    geom_segment(data = quantframe_list, aes(x = x, xend = xend, y = Inf, yend = Inf)) +
    geom_text(
      data = quantframe_list, aes(value, y = Inf, label = name),
      angle = 90, size = 8 * 5 / 14,
      hjust = c(rep(c(0, 1), length(quantframe_list$value) / 2), 0)
    ) +
    scale_color_manual(values = maia_palette, guide = FALSE) +
    scale_y_continuous(breaks = c(0,5,10)) +
    labs(y = "Count of normals") +
    coord_cartesian(ylim = c(0, 10), clip = "off") +
    facet_wrap(~type, scales = "free_x") +
    theme_min() +
    theme(
      plot.margin = margin(t = 20, unit = "pt"),
      strip.text = element_text(margin = margin(b = 30, unit = "pt"), hjust = 0, size = 11)
    )
  p
}

#' plot_PSD
#' @name plot_PSD
#' @description plotting relative percentile of estimated Mean deviation for tested observers
#' @param testdata test data. Do not try to pass a list. Built to pass the output from [read_maia] or, even better, from [compare_norm]
#' If data is organised in list of data frames, bind those first.
#' @param quantiles Quantiles for normal values to be displayed.
#' @return List of plots which are bound together with the patchwork package.
#' @author tjebo
#' @examples
#' # testdata <- read_maia(folder = file.path(getwd(), "data-raw"))
#' # comparedat <- compare_norm(testdata)
#' # plot_PSD(norm_data)
#' @export
plot_PSD <- function(testdata, quantiles = 0.1) {

  PSD_list <- MD_PSD(testdata) %>%
    split(.$testtype) %>%
    bind_rows(.id = "type") %>%
    mutate(type = factor(type, levels = c("mesopic", "red", "cyan", "cr_diff")))

  unique_type <- unique(testdata$testtype)
  names(unique_type) <- unique_type

  list_norm_PSD <- lapply(unique_type, function(x) summary_MDPSD[[x]])
  norm_PSD <- list_norm_PSD %>%
    bind_rows(.id = "type") %>%
    mutate(type = factor(type, levels = c("mesopic", "red", "cyan", "cr_diff")))

  quantframe_list <- lapply(list_norm_PSD, function(x) enframe(stats::quantile(x$PSD, probs = c(seq(0, 1, by = quantiles))))) %>%
    bind_rows(.id = "type") %>%
    mutate(type = factor(type, levels = c("mesopic", "red", "cyan", "cr_diff"))) %>%
    group_by(type) %>%
    mutate(x = min(value), xend = max(value)) %>%
    ungroup()

  p <-
    ggplot(norm_PSD, aes(PSD)) +
    geom_histogram(binwidth = 0.3, fill = NA, color = "grey") +
    geom_segment(
      data = PSD_list, aes(x = PSD, xend = PSD, y = Inf, yend = -Inf, color = type),
      size = 1.5, alpha = 0.5
    ) +
    geom_text(
      data = PSD_list, aes(x = PSD, y = 9, label = testID, color = type),
      size = 8 * 5 / 14,
      alpha = 0.5, hjust = 1, angle = 90, vjust = 0
    ) +
    geom_segment(data = quantframe_list, aes(x = x, xend = xend, y = Inf, yend = Inf)) +
    geom_text(
      data = quantframe_list, aes(value, y = Inf, label = name),
      angle = 90, size = 8 * 5 / 14,
      hjust = c(rep(c(0, 1), length(quantframe_list$value) / 2), 0)
    ) +
    scale_color_manual(values = maia_palette, guide = FALSE) +
    scale_y_continuous(breaks = seq(0, 15, 5)) +
    labs(y = "Count of normals") +
    coord_cartesian(ylim = c(0, 17.5), clip = "off") +
    facet_wrap(~type, scales = "free_x") +
    theme_min() +
    theme(
      plot.margin = margin(t = 20, unit = "pt"),
      strip.text = element_text(margin = margin(b = 30, unit = "pt"), hjust = 0, size = 11)
    )
  p
}
