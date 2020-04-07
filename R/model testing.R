# data_model %>%
#   ggplot(aes(as.numeric(position_fac), MeanSens))+
#   geom_point(alpha = 0.1)+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~testtype)+
#   theme(axis.text.x = element_text(angle = 90))
#
# data_model %>% split(.$testtype) %>%
#   map(.f = function(x) summary(lm(formula = MeanSens ~ as.numeric(position_fac) +age, data = x)))
#
#
# data_summ <- data_model %>%
#   group_by(patID, testtype) %>%
#   summarise(meansens = mean(MeanSens, na.rm = TRUE), age = mean(age), lens = unique(lens))
#
# data_summ %>%
#   ggplot(aes(age, meansens, color = lens))+
#   geom_point(alpha = 0.1)+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~testtype)+
#   theme(axis.text.x = element_text(angle = 90))
#
# data_summ %>% split(.$testtype) %>%
#   map(.f = function(x) summary(lm(formula = meansens ~ age, data = x)))
#
#
