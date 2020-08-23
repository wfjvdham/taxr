#' add_total_tax
#'
#' @param df tibble to add column to
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
add_total_tax <- function(df) {
  df %>%
    dplyr::mutate(
      total_tax = income_tax + zvw_bijdrage + tariefsaanpassing_extra -
        alg_hef_korting - arb_korting
    )
}
