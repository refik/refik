#' Credit card expense data
#'
#' @export
credit_card <- function() {
  category <- airtable(base_name = "banking", table_name = "category")
  category_name <- setNames(category$name, category$id)

  airtable(base_name = "banking", table_name = "credit_card") %>%
    dplyr::mutate(date = as.Date(.data$date)) %>%
    dplyr::mutate(term_month = as.integer(.data$term_month)) %>%
    airtable_link_normalize("category") %>%
    dplyr::mutate(category = category_name[.data$category]) %>%
    dplyr::select("year", "term_month", "date", "description", "currency",
                  "amount", "category")
}
