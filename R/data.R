
#' Norm data
#'
#' Data obtained from healthy controls. A broad range of ages were used.
#' It contains data of \Sexpr{length(unique(interaction(microperimetr::norm_data$patID, microperimetr::norm_data$eye)))} eyes of \Sexpr{length(unique(microperimetr::norm_data$patID))} observers.
#'
#' DOCUMENTATION TBC
#'
#' \itemize{
#'    \item patID
#'    }
#'
#'
#' @docType data
#' @name norm_data
#' @usage data(norm_data)
#' @format A data frame with \Sexpr{nrow(microperimetr::norm_data)} rows and NN variables:
#' @source University Eye Hospital Bonn. Collected by Simone Tzaridis and Maximilian Pfau
#'
NULL

#' Testdata from a single maia test
#' @docType data
#' @name testdat1
#'
NULL

#' Testdata output of read_maia of several tests
#' @docType data
#' @name testdata
#'
NULL

#' Coefficient of repeatability of sMAIA device
#' @docType data
#' @description Calculated with formula
#'   \eqn{CoR = 1.96* \sqrt2*\sqrt(within-subject-variance)}
#' @name cor_maia
#'
NULL
