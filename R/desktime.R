#' Daily program usage from Desktime api
#'
#' @export
desktime_api_day <- function(date = Sys.Date()) {
  api_url <- "https://desktime.com/api/v2/json/employee" %>%
    urltools::param_set("id", get_config("desktime", "employee_id")) %>%
    urltools::param_set("apiKey", get_config("desktime", "api_key")) %>%
    urltools::param_set("date", date)

  api_response <- jsonlite::fromJSON(api_url)
  assert_that(assertthat::has_name(api_response, "apps"))

  app_type <- c("-1" = "unproductive",
                "0"  = "neutral",
                "1"  = "productive")

  # Converting app productivity caregory to human readable form
  names(api_response$apps) <- app_type[names(api_response$apps)]

  # Converting the json returned by API to tibble
  api_response$apps %>%
    purrr::map_df(function(app_records) {
      purrr::map(app_records, ~ {
        # Some category_id are character, some are integer. This
        # causes an error on bind_rows. Preventing that.
        .x$category_id <- as.integer(.x$category_id)
        .x
      }) %>%
        dplyr::bind_rows()
    },
    .id = "productivity") %>%
    dplyr::mutate(date = !!date)
}

#' Desktime api data for a given period
#'
#' @export
desktime_api_period <- function(period_start, period_end = Sys.Date()) {
  day_range(period_start, period_end) %>%
    purrr::map_df(~ desktime_api_day(date = .x))
}


