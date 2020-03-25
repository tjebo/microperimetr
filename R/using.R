#' using
#' @description internal function for test of package presence.
#' @source https://stackoverflow.com/a/44660688/7941188
#' @author SO user Matthew https://stackoverflow.com/users/4125693/matthew
#'
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[req == FALSE]
  n <- length(need)
  if (n > 0) {
    libsmsg <- if (n > 2) paste(paste(need[1:(n - 1)], collapse = ", "), ",", sep = "") else need[1]
    print(libsmsg)
    if (n > 1) {
      libsmsg <- paste(libsmsg, " and ", need[n], sep = "")
    }
    libsmsg <- paste("The following packages could not be found: ", libsmsg, "\n\r\n\rInstall missing packages?", collapse = "")
    if (winDialog(type = c("yesno"), libsmsg) == "YES") {
      install.packages(need)
      lapply(need, require, character.only = TRUE)
    }
  }
}
