#' add_carts
#' @name add_carts
#' @description Adding cartesian coordinates to test results
#' @param data Data frame, ideally output from [read_maia_tgz].
#' @param digits how precise to be about eccentricity and angles
#' @return Data frames
#' @details
#' if not using output from read_maia_tgz, the data frame needs to contain a
#' column called "angle" (angle given in radians) and "eccent" (eccentricity given in any unit)
#'
#' @examples
#' lm_dat <- lm_loc(testdata = testdat1)
#' preddat <- predict_norm(testdat1, list_model = lm_dat)
#' testdat_coord <- coord_cart(testdat1)
#' interpol_dat <- interpolate_norm(preddat, newgrid = testdat_coord)
#' @family prediction functions
#' @export
add_carts <- function(data, digits = 2) {
  data_coord <- data %>%
  mutate(
    x = round(cos(.data$angle * pi / 180) * as.numeric(.data$eccent), digits = digits),
    y = round(sin(.data$angle * pi / 180) * as.numeric(.data$eccent), digits = digits)
  )
return(data_coord)
}

## for plotting function
# test_d <- filter(test, testDate>"2020-01-01") %>%
#   add_carts()
# ggplot(test, aes(x,y)) +
#   geom_point()

