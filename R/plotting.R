#' plot_MD
#' @name plot_MD
#' @description plotting relative percentile of estimated Mean deviation for tested observers
#' @param testdata test data. Do not try to pass a list. Ideally, pass the output from [read_maia]
#' If data is organised in list of data frames, bind those first.
#' @return List of plots which are bound together with the patchwork package.
#' @author tjebo
#' @examples
#' testdata <- read_maia()
#' plot_MD(testdata)
#' @export
plot_MD <- function(testdata){
microperimetR:::using('patchwork')
  MD_list <- MD_PSD(testdata) %>% split(.$testtype)

unique_type <- unique(testdata$testtype)
names(unique_type) <- unique_type

list_norm_MD <- lapply(unique_type, function(x) microperimetR:::summary_MDPSD[[x]] )

quantframe_list <- lapply(list_norm_MD, function(x) enframe(quantile(x$MeanDev, probs = c(seq(0,1, by = 0.05)))))

list_plots <- list()
for(i in unique_type){
p1 <- ggplot(list_norm_MD[[i]], aes(MeanDev)) +
  geom_histogram(binwidth = 0.3, fill = NA, color = "grey") +
  theme_min()+
  theme(plot.margin = margin())

p2 <- ggplot(list_norm_MD[[i]], aes(MeanDev)) +
  geom_segment(
    data = MD_list[[i]], aes(x = MeanDev, xend = MeanDev, y = -Inf, yend = 0),
    color = maia_palette[[i]], size = 2, alpha = 0.5
  ) +
  geom_text(
    data = MD_list[[i]], aes(x = MeanDev, y = -0.5, label = testID),
    color = maia_palette[[i]], size = 8 * 5 / 14,
    alpha = 0.5, hjust = 1, angle = 90, vjust = 0
  ) +
  annotate(
    geom = "segment", x = min(list_norm_MD[[i]]$MeanDev, na.rm = TRUE),
    xend = max(list_norm_MD[[i]]$MeanDev, na.rm = TRUE), y = 0, yend = 0
  ) +
  annotate(
    geom = "text", x = quantframe_list[[i]]$value, y = 0, label = quantframe_list[[i]]$name,
    angle = 90, size = 8 * 5 / 14, hjust = c(rep(c(0, 1), length(quantframe_list[[i]]$value) / 2), 0)
  ) +
  annotate(
    geom = "text", x = -Inf, y = 0, label = 'Percentile',
    angle = 90, size = 10 * 5 / 14, hjust = 0.5, vjust = 0
  ) +
  coord_cartesian(ylim = c(-1,1), clip = 'off') +
  theme_void() +
  theme(plot.margin = margin(b = -0.5, unit = 'in'))

list_plots[[i]] <- list(p2,p1)

}
plot_list <- lapply(list_plots, function(x) patchwork::wrap_plots(x,nrow = 2, heights = c(1,0.5)))
plots <- patchwork::wrap_plots(plot_list)
plots
}

