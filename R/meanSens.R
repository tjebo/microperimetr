#' Microperimetry basic statistics.
#' @name meanSens
#' @description Calculates mean sensitivity (MS) and standard deviation (SD) from
#'   all tests within a data frame containing microperimetry test data.
#' @param data microperimetry test data, ideally import with read_maia_tgz
#' @param global TRUE (MS and SD of all tests will be calculated) or
#'   FALSE (default), where MS and SD will be provided for each test separately
#' @param na.rm TRUE (default): NA will be ignored. False: will return NA
#' @param digits round to how many digits.
#' @return if global = TRUE: vector of MS and SD, else dataframe (or: tibble)
#' @export

meanSens <- function(data, global = FALSE, na.rm = TRUE, digits){
  if(!global){
  res <-
    data %>%
    group_by(testID) %>%
    summarise(meanSens = mean(value, na.rm = na.rm),
                sd = sd(value, na.rm = na.rm)) %>%
    mutate(across(c(meanSens, sd), ~round(.x, digits = digits))) %>%
    as.data.frame()
  } else {
  res <- round(c(meanSens = mean(data$value, na.rm = na.rm),
           sd = sd(data$value, na.rm = na.rm)), digits = digits)
  }
  return(res)
}
