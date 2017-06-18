#' Get configuration parameters
#'
#' @export
get_config <- function(namespace, key) {
  value <- airtable(base_name = "config", table_name = "config") %>%
    dplyr::filter(.data$namespace == !!namespace,
                  .data$key == !!key) %>%
    dplyr::pull(.data$value)

  assert_that(assertthat::is.string(value),
              msg = glue("\"{key}\" config value is not a string"))

  value
}
