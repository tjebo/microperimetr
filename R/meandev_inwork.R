# library(microperimetR)
# testdat <- read_maia()
#
# test <- MD_PSD(testdat)
#
# ecdf_MDmes <- ecdf_MD()[['mesopic']]
#
# norm_MD_mes <- microperimetR:::summary_MDPSD[['mesopic']]
# test$perc <- ecdf_MDmes(test$MeanDev)*100
# data.frame(lapply(testVect, type.convert), stringsAsFactors=FALSE)
# quantframe <- enframe(quantile(norm_MD_mes$MeanDev, probs = c(seq(0,1, by = 0.025))))
# 1:(length(quantframe$value)/2)
# rep(c(0,1), length(quantframe$value)/2)
# ggplot(norm_MD_mes, aes(MeanDev)) +
#   geom_histogram(binwidth = 0.2, fill = NA, color = 'grey') +
#   geom_segment(data = test, aes(x = MeanDev, xend = MeanDev, y = -2, yend = 0.5),
#                color = maia_palette['mesopic'], size = 2, alpha = 0.5) +
# coord_cartesian(clip = 'off', ylim = c(0, 6)) +
#   annotate(geom = 'segment', x = min(norm_MD_mes$MeanDev, na.rm = TRUE),
#            xend = max(norm_MD_mes$MeanDev, na.rm = TRUE), y = -2, yend = -2) +
#   annotate(geom = 'text', x = quantframe$value, y = -2, label = quantframe$name ,
#            angle = 90, size = 8*5/14, hjust = c(rep(c(0,1), length(quantframe$value)/2), 0)) +
#   theme_min() +
#     theme(plot.margin = margin(b = 4, unit = 'lines'))
