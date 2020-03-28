#' get_age
#' @name get_age
#' @description
#' convenience function - calculate age in years.
#' modified from: https://stackoverflow.com/a/47529507/7941188
#'
#' @author Stackoverflow::Moody_Mudskipper
#' @param from_date beginning of time span. Takes dates as date or as character class
#' @param to_date end (default: now!)
#' @param dec decide if you want to see decimals (default: FALSE)
#' @return Data frame
#'
#' @export

get_age <- function(from_date, to_date = lubridate::now(), dec = FALSE) {
  microperimetR:::using('lubridate')
  if (is.character(from_date))
    from_date <- lubridate::as_date(from_date)
  if (is.character(to_date))
    to_date <- lubridate::as_date(to_date)
  if (dec) {
    age <- lubridate::interval(start = from_date, end = to_date)/lubridate::years(1)
  }
  else {
    age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date,
                                                                    end = to_date)))
  }
  age
}
