#' Day range for a given period
#'
#' @export
day_range <- function(day_start, day_end) {
  day_count <- as.integer(difftime(day_end, day_start, units = "day"))
  purrr::map(seq(1, day_count), ~ day_start + lubridate::days(.x))
}
