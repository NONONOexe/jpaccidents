#' Merge accident data
#'
#' `merge_accident_data()` merges multiple `accident_data` objects into a
#' single `accident_data` object.
#'
#' @param ... `accident_data` objects to merge.
#' @return A merged `accident_data` object.
merge_accident_data <- function(...) {
  UseMethod("merge_accident_data")
}

#' @rdname merge_accident_data
#' @export
merge_accident_data.accident_data <- function(...) {
  merge_accident_data(list(...))
}

#' @rdname merge_accident_data
#' @keywords internal
merge_accident_data.list <- function(data_list, ...) {
  data_list %>%
    purrr::transpose() %>%
    purrr::map(dplyr::bind_rows) %>%
    as_accident_data()
}
