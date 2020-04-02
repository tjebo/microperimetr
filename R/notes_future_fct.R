# #library(robustbase)
#
# # Sn factor
# # calculation of dispersion, e.g. useful for calculation of fixation stability.
#
# library(microperimetR)
# library(tidyverse)
# testdat <- read_maia(folder = file.path(getwd(), "data-raw"))
# testdat1 <- testdat %>% filter(testID == 834)
#
# id_cols <- testdat1[sapply(names(testdat1), function(x) {length(unique(testdat1[[x]])) == 1})]
# id_cols <- lapply(id_cols, unique)
# variable_cols <-  testdat1[sapply(testdat1, function(x) {length(unique(x)) != 1})]
#
# list_dat <- c(variable_cols, id_cols)
# object_size(testdat1)
# object_size(list_dat)
# lapply()
# list_dat <- structure(.Data = list_dat, class = 'mp')
#
# print.mp <- function(x){
#   print(head(x$data, 10))
#   print('...')
#   print(unlist(x[-1]))
# }
# print(list_dat)
# list_dat$patID
# names(list_dat)
