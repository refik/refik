#' @importFrom dplyr %>%
#' @importFrom dplyr if_else
#' @importFrom rlang .data quo
#' @importFrom assertthat assert_that
#' @importFrom glue glue
NULL

#' Get data from Airtable
#'
#' @export
airtable <- memoise::memoise(function(base_name = NULL, table_name = NULL,
                                      base_id = NULL,
                                      single_link = character(0),
                                      datetime = character(0),
                                      boolean = character(0),
                                      date = character(0)) {
  assert_that(!is.null(base_name) || !is.null(base_id),
              msg = "One of base_name or base_id must be provided")

  # Bootstrapping for getting config base. Other base_id's will be retrieved
  # from the config Airtable
  if (base_name == "config") base_id <- "app8tM2wDLB6vXtS0"

  if (is.null(base_id)) {
    # If base_name is given, base_id must be found from the base dataset
    base_id <- airtable(base_name = "config", table_name = "base") %>%
      dplyr::filter(.data$base_name == !!base_name) %>%
      dplyr::pull(.data$base_id)

    assert_that(assertthat::is.string(base_id))
  }

  # AirtableR doesn't url encode so tables with Turkish characters cause problem
  table_name <- utils::URLencode(table_name)

  # Getting the table api interface
  table_api <- AirtableR::Airtable(base_id, table_name)[[table_name]]

  # AirtableR returns the data as a list by default. Airtable API has
  # pagination and recursive makes sure all records are retreived
  records_list <- table_api$list_records(recursive = TRUE)

  # Converting list output to tibble
  if (length(records_list) == 0) {
    # If the list is empty, don't bother with conversion
    return(tibble::tibble())
  }

  # All data has created_time by default. Making sure it gets converted to
  # proper datetime object
  if (!("created_time" %in% datetime)) datetime <- c("created_time", datetime)
  browser()
  # Converting the list to tibble
  purrr::map(records_list, ~ c(
    purrr::map(.x$fields, ~ ifelse(is.list(.x), list(as.character(.x)), .x)),
    "airtable_id" = .x$id,
    "airtable_created_time" = .x$createdTime
  )) %>%
    dplyr::bind_rows() %>%

    # Converting common data types to tidy objects
    dplyr::mutate_at(datetime, ~ lubridate::ymd_hms(.x, tz = "Turkey")) %>%
    dplyr::mutate_at(date , ~ as.Date(.x)) %>%
    dplyr::mutate_at(boolean, ~ if_else(is.na(.x), FALSE, TRUE)) %>%
    dplyr::mutate_at(single_link, function(link) {
      purrr::map(link, ~ if_else(is.null(.x), NA_character_, .x)) %>%
        as.character()
    })
})
