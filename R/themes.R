#' theme_tjebo
#' @name theme_min
#' @description very basic theme, modified theme_bw
#' @author Tjebo F.C. Heeren
#' @param base_size	 base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @export
theme_min<- function (base_size = 10, base_family = "", base_line_size = base_size/30,
                         base_rect_size = base_size/30) {
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5, size = rel(1)),
          axis.line = element_line(colour = "black", size = rel(1)),
          axis.text = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.key.size = unit(0.8*base_size,'pt'),
          complete = TRUE)
}
