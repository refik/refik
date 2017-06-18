#' @importFrom dplyr %>%
#' @importFrom dplyr if_else
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#' @importFrom glue glue
NULL

#' Get data from Airtable
#'
#' @export
airtable <- memoise::memoise(function(base_name = NULL, table_name = NULL,
                                      base_id = NULL, tr_base = FALSE,
                                      as_tibble = TRUE) {
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

  if (as_tibble == TRUE) {
    # Converting list output to tibble
    if (length(records_list) == 0) {
      # If the list is empty, don't bother with conversion
      result <- tibble::tibble()
    } else {
      # Converting the list to tibble
      result <- purrr::map(records_list, ~ c(
        purrr::map(.x$fields, ~ {
          if (is.list(.x)) list(as.character(.x))
          else .x
        }),
        "id" = .x$id,
        "created_time" = .x$createdTime
      )) %>%
        dplyr::bind_rows()
    }
  } else {
    result <- records_list
  }

  result
})

#' Normalizing airtable linked columns
#'
#' @export
airtable_link_normalize <- function(dataset, column) {
  # Linked columns are list by default, converting to character
  dplyr::mutate(dataset, !!column := as.character(
    purrr::map(.data[[column]], ~ {
      # Linked columns have NULL values by default, converting to character NA
      # because it will be cast to character vector later
      if (is.null(.x)) NA_character_
      else .x
    })
  ))
}
