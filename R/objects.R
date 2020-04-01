#' maia_palette
#' @description color palette for maia colors
#' @export
maia_palette <- c(red = '#ca0020', cyan = '#2166ac', mesopic = 'Grey35', cr_diff = '#de77ae')

#' ecdf_MD
#' @name ecdf_MD
#' @description calculates empirical distribution of estimated mean deviation
#' @return list for each test type
#'
ecdf_MD <- function() lapply(summary_MDPSD, function(x) stats::ecdf(x$MeanDev))

#' ecdf_PSD
#' @name ecdf_PSD
#' @description calculates empirical distribution of estimated pattern standard deviation
#' @return list for each test type
ecdf_PSD <- function() lapply(summary_MDPSD, function(x) stats::ecdf(x$PSD))
