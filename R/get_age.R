#' get the age (time difference in years)
#'
#' The function will extract the data from the raw .tgz output of your MAIA.
#'
#' @author Stackoverflow :: Moody Mudskipper
#'
#' @param from_date beginning of time span
#' @param to_date end (default: now!)
#' @param dec decide if you want to see decimals (default: FALSE)
#'
#' @return Data frame
#'
#' @import lubridate
#'
#' @export

get_age <- function (from_date, to_date = lubridate::now(), dec = FALSE)
{
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
